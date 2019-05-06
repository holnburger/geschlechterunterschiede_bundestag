# Setup

library(tidyverse)
library(car)
library(apa)
library(knitr)
library(kableExtra)

# Gender-Dictionär (modifiziert) laden
gend_dict <- read_csv("data/gender_dict_modified.csv")

# Daten der Abgeordneten laden
mdb_data <- read_rds("data/mdb_data.RDS")
mdb_overview <- read_rds("data/BT_19/overview.RDS")

# Reden jeweils zusammengefasst, ohne Unterbrechungen und Zwischenrufe
mdb_speeches <- read_rds("data/BT_19/speeches.RDS")

mdb_full_speeches <- mdb_overview %>%          # Übersicht der Reden
  filter(!is.na(redner_fraktion)) %>%              # Nur Reden von MdBs
  left_join(mdb_data %>% select(id, geschlecht), by = c("redner_id" = "id")) %>%
  left_join(mdb_speeches, by = "rede_id") %>%  # Mit den anderen Daten verknüpfen
  filter(typ != "kommentar") %>%               # Keine Kommentare (Zwischenrufe)
  filter(präsidium == FALSE) %>%               # Keine Kommentare des Präsidiums (Aufrufe)
  group_by(rede_id) %>%                        # Komprimieren auf eine Rede pro Zeile
  mutate(rede_full = paste0(rede, collapse = "\n ")) %>%
  distinct(rede_id, .keep_all = TRUE) %>%
  mutate(woerter = str_count(rede_full, "\\w+")) %>% # Anzahl der Wörter in einer Rede
  select(rede_id, redner_id, redner_vorname, redner_nachname, redner_fraktion, sitzung, datum, wahlperiode,
         geschlecht, rede_full, woerter)

# Reden speichern
write_rds(mdb_full_speeches, "data/BT_19/full_speeches.RDS")

# Daten sind nun fertig aufbereitet.
# Abgleich der vewendeten, nicht Geschlechterinklusiven Wörter pro Rede über Gender-Diktionär

# Zunächst: Tabelle über Genderinklusive Wörter generieren, für Veranschaulichung in der Arbeit
# Zufällige Wörter, mit Seed, damit auch in Zukunft immer die gleichen Wörter
set.seed(123)
gend_dict %>%
  rename("Genderexklusive Begriffe" = word_list, "Genderinklusive Varianten" = alternative_list) %>%
  sample_n(10) %>%
  knitr::kable(format = "latex", booktabs = TRUE, linesep = "") %>%
  write_file(., "document/tables/genderinklusive_woerter_beispiel.tex")
# Es handelt sich nur um Beispiele, die auch zeigen, dass das Wörterbuch nicht ideal ist
# Beispiel: Schüler --> Kinder. Hier macht es einen eklatanten Unterschied,
# Es gibt auch üblicherweise mehr als einen erseztenden Begriff (Lehrling --> Azubi).

# Allerdings findet man nicht nur GFL Ansprache über das Diktionär, sondern auch über "Kolleginnen und Kollegen"
# "Arzt und Ärztin". Wir versuchen, über regular expressions diese "Floskeln" zu finden.

mdb_gfl_phrases <- mdb_full_speeches %>%                                  # extrahiert alle mit "und" oder "oder" getrennten Wörter
  mutate(gfl_phrases = str_extract_all(rede_full, "(\\w+\\sund\\s\\w+|\\w+\\soder\\s\\w+)")) %>%     
  select(rede_id, gfl_phrases) %>%                                     # wir brauchen nur die rede_id und die Wörter
  unnest(gfl_phrases) %>%                                              # wir suchen nur Kombinationen, die mit *innen oder *in Enden
  mutate(gfl_detect_female = str_detect(gfl_phrases, "\\w{4,}(in(\\s|$)|innen(\\s|$))")) %>%  
  # wobei mindestens 4 Buchstaben vor dem *in/innen kommen müssen
  mutate(gfl_detect_male = str_detect(gfl_phrases, "\\w{4,}(e(\\s|$)|er(\\s|$)|en.*?(en))")) %>%
  # Erkennt *e und *er am Ende, und wenn zwei mal *en vorkommt (Kolleginnen und Kollegen).
  # Als nächstes: Nur wenn detect_male und detect_female TRUE ist, ist es genderinklusiv
  mutate(gfl_true = case_when(gfl_detect_female == TRUE & gfl_detect_male == TRUE ~ TRUE,
                              TRUE ~ FALSE)) %>%
  select(rede_id, gfl_true, gfl_phrases) %>%
  filter(gfl_true == TRUE) %>%
  group_by(gfl_phrases) %>%
  count(gfl_phrases, sort = TRUE)

write_csv(mdb_gfl_phrases %>% select(gfl_phrases), "data/gfl_phrases.csv")
# mdb_gfl_phrases <- read_csv("data/gfl_phrases.csv")

# Auch hier muss die Tabelle wieder manuell angepasst werden, da dies nicht immer funktioniert.
# Die modifizierte Tabelle wird geladen und anschließend zufällige Beispiele gezeigt.

gfl_modified_phrases <- read_csv("data/gfl_phrases_modified.csv")

gfl_modified_phrases %>%
  sample_n(10) %>%
  rename(Formulierung = gfl_phrases) %>%
  knitr::kable(format = "latex", booktabs = TRUE, linesep = "") %>%
  write_file(., "document/tables/genderinklusive_formulierungen_zufall.tex")

# Zusammenfassende Tabelle aller Begriffe und Formulierungen

dict_summary_table <- tibble("Genderinklusive Formulierungen" = nrow(gfl_modified_phrases), 
  "Genderinklusive Begriffe" = nrow(genderinkl_begriffe), 
  "Genderexklusive Begriffe" = nrow(genderexkl_begriffe)) 

dict_summary_table %>%
  gather() %>% 
  knitr::kable(format = "latex", booktabs = TRUE, linesep = "",
               col.names = c("", "Anzahl")) %>%
  write_file(., "document/tables/genderinklusive_woerter_anzahl.tex")

