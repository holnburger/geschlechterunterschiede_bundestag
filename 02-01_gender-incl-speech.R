# Setup

library(tidyverse)
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
  filter(is.na(redner_rolle)) %>%              # Nur Reden von MdBs
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

# Zwei Characterstrings: Einer zum Erkennen genderexklusiver Begriffe, einer für genderinklusvie Begriffe

genderexkl_begriffe <- gend_dict %>%
  select(word_list) %>%
  distinct(word_list)

genderinkl_begriffe <- gend_dict %>%
  select(alternative_list) %>%
  distinct(alternative_list)

# Tabelle um zu zeigen, auf wie viele Begriffe überprüft wird
tibble("Genderexklusive Begriffe" = nrow(genderexkl_begriffe),
  "Genderinklusive Begriffe" = nrow(genderinkl_begriffe)) %>%
  knitr::kable(format = "latex", booktabs = TRUE, linesep = "") %>%
  write_file(., "document/tables/genderinklusive_woerter_anzahl.tex")

# Test der Hypothese: Frauen benutzen eher genderinklusive Sprache als Männer.
# Test über: Anteil genderinklusiver Sprache in den Reden von Frauen und Männern. Anteil über Wörter.

# Diese Auswertung dauert sehr lange. Circa eine Stunde für die Auswertung.
mdb_gfl_speeches <- mdb_full_speeches %>%
  mutate(rede_full = tolower(rede_full)) %>%
  mutate(genderexkl_words = 
           str_extract_all(rede_full,
                           paste(tolower(genderexkl_begriffe$word_list), collapse = "|"))) %>%
  mutate(genderexkl_anzahl = lengths(genderexkl_words)) %>%
  mutate(genderinkl_words = 
           str_extract_all(rede_full,
                           paste(tolower(genderinkl_begriffe$alternative_list), collapse = "|"))) %>%
  mutate(genderinkl_anzahl = lengths(genderinkl_words)) %>%
  arrange(-genderinkl_anzahl)

write_rds(mdb_gfl_speeches, "data/BT_19/gfl_speeches.RDS")
# mdb_gfl_speeches <- read_rds("data/BT_19/gfl_speeches.RDS")

# Allerdings findet man nicht nur GFL Ansprache über das Diktionär, sondern auch über "Kolleginnen und Kollegen"
# "Arzt und Ärztin". Wir versuchen, über regular expressions diese "Floskeln" zu finden.

mdb_gfl_ansprache <- mdb_gfl_speeches %>%                                # extrahiert alle mit "und" oder "oder" getrennten Wörter
  mutate(gfl_ansprache = str_extract_all(rede_full, "(\\w+\\sund\\s\\w+|\\w+\\soder\\s\\w+)")) %>%     
  select(rede_id, gfl_ansprache) %>%                                     # wir brauchen nur die rede_id und die Wörter
  unnest(gfl_ansprache) %>%                                              # wir suchen nur Kombinationen, die mit *innen oder *in Enden
  mutate(gfl_detect_female = str_detect(gfl_ansprache, "\\w{4,}(in(\\s|$)|innen(\\s|$))")) %>%  
  # wobei mindestens 4 Buchstaben vor dem *in/innen kommen müssen
  mutate(gfl_detect_male = str_detect(gfl_ansprache, "\\w{4,}(e(\\s|$)|er(\\s|$)|en.*?(en))")) %>%
  # Erkennt *e und *er am Ende, und wenn zwei mal *en vorkommt (Kolleginnen und Kollegen).
  # Als nächstes: Nur wenn detect_male und detect_female TRUE ist, ist es genderinklusiv
  mutate(gfl_true = case_when(gfl_detect_female == TRUE & gfl_detect_male == TRUE ~ TRUE,
                              TRUE ~ FALSE)) %>%
  select(rede_id, gfl_true, gfl_ansprache) %>%
  filter(gfl_true == TRUE) %>%
  group_by(gfl_ansprache) %>%
  count(gfl_ansprache, sort = TRUE)

write_csv(mdb_gfl_ansprache, "data/gfl_ansprache_dictionär.csv")
  



write_rds(mdb_exkl_words, "data/BT_19/genderexklusive_words.RDS")

# Wie hoch ist der Anteil an genderinklusiven Wörtern pro Rede in Wörtern? Im Vergleich zu "nicht-genderinklusiven" Begriffen?

mdb_inkl_words %>%
  filter(!is.na(geschlecht)) %>%
  mutate(rel_inkl_words = genderinkl_words/woerter) %>%
  arrange(-rel_inkl_words) %>%
  View("wat")