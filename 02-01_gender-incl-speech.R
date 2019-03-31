library(tidyverse)
library(knitr)
library(kableExtra)

# Gender-Dictionär (modifiziert) laden
gend_dict <- read_csv("data/gender_dict_modified.csv")

# Daten der Abgeordneten laden
mdb_data <- read_rds("data/mdb_data.RDS")
mdb_overview <- read_rds("data/BT_19/overview.RDS")

# Korrekturdaten
# Carsten Träger und Marja-Lisa Völlers haben teilweise falsche Angaben zur ID in den Protokollen

corr_traeger <- mdb_data %>%
  filter(id == "11004426") %>%
  mutate(id = "999190001")

corr_voellers <- mdb_data %>%
  filter(id == "11004942") %>%
  mutate(id = "10000")

mdb_data <- mdb_data %>%
  bind_rows(corr_traeger) %>%
  bind_rows(corr_voellers)


# Reden jeweils zusammengefasst, ohne Unterbrechungen und Zwischenrufe
mdb_speeches <- read_rds("data/BT_19/speeches.RDS") %>%
  left_join(mdb_overview %>% select(rede_id, redner_rolle),
            by = "rede_id") %>%                # Nur die Reden von MdBs
  filter(is.na(redner_rolle)) %>%
  filter(typ != "kommentar") %>%               # Nur die Reden ohne Kommentare
  group_by(rede_id) %>%                        # Eine Rede pro Row
  mutate(rede_full = paste0(rede, collapse = "\n "))%>%
  distinct(rede_id, rede_full, id, vorname, nachname, präsidium) %>%
  filter(präsidium == FALSE) %>%               # Keine Kommentare des Präsidiums
  left_join(mdb_overview, by = "rede_id") %>%  # Redner-ID abgleich
  mutate(woerter = str_count(rede_full, "\\w+")) %>%       # Anzahl der Wörter pro Rede
  filter(woerter > 20) %>%                     # Mindestens 20 Wörter in einer Rede
  left_join(mdb_data %>%
              select(id, geschlecht), by = "id") # Abgleich mit den Stammdaten für Geschlechtsangaben

# Daten sind nun fertig aufbereitet.
# Abgleich der vewendeten, nicht Geschlechterinklusiven Wörter pro Rede über Gender-Diktionär

# Zunächst: Tabelle über Genderinklusive Wörter generieren, für Veranschaulichung in der Arbeit
# Zufällige Wörter, mit Seed, damit auch in Zukunft immer die gleichen Wörter
set.seed(42)
gend_dict %>%
  rename("Übliche Begriffe" = word_list, "Genderinklusive Varianten" = alternative_list) %>%
  sample_n(10) %>%
  knitr::kable(format = "latex", booktabs = TRUE, linesep = "") %>%
  write_file(., "document/tables/genderinklusive_woerter_beispiel.tex")
# Es handelt sich nur um Beispiele, die auch zeigen, dass das Wörterbuch nicht ideal ist
# Beispiel: Mutterschutz --> Elternschutz. Hier macht es einen eklatanten Unterschied,
# Es gibt auch üblicherweise mehr als einen erseztenden Begriff (Lehrling --> Azubi).

# Wie viele "Übliche Begriffe" und wie viele genderinklusive Begriffe enthalten die jeweiligen
# Reden.

# Zwei Characterstrings: Einer zum Erkennen üblicher Begriffe, einer für genderinklusvie Begriffe

uebliche_begriffe <- gend_dict %>%
  select(word_list) %>%
  distinct(word_list)

genderinkl_begriffe <- gend_dict %>%
  select(alternative_list) %>%
  distinct(alternative_list)

# Tabelle um zu zeigen, auf wie viele Begriffe überprüft wird
tibble("Übliche Begriffe" = nrow(uebliche_begriffe),
  "Genderinklusive Begriffe" = nrow(genderinkl_begriffe)) %>%
  knitr::kable(format = "latex", booktabs = TRUE, linesep = "") %>%
  write_file(., "document/tables/genderinklusive_woerter_anzahl.tex")

# Test der Hypothese: Frauen benutzen eher genderinklusive Sprache als Männer.
# Test über: Anteil genderinklusiver Sprache in den Reden von Frauen und Männern. Anteil über Wörter.
# !! Problem: Case Sensitiv! Ändern. Und er findet noch komische Daten.

# Diese Auswertung dauert sehr lange. Circa eine Stunde für die Auswertung.
mdb_inkl_words <- mdb_speeches %>%
  mutate(genderinkl_words = str_count(rede_full, paste(genderinkl_begriffe$alternative_list, collapse = "|"))) %>%
  select(rede_id, geschlecht, genderinkl_words, woerter, rede_full) %>%
  mutate(uebliche_words = str_count(rede_full, paste(uebliche_begriffe$word_list, collapse = "|"))) %>%
  arrange(-genderinkl_words)

write_rds(mdb_inkl_words, "data/BT_19/genderinclusive_words.RDS")

# Wie hoch ist der Anteil an genderinklusiven Wörtern pro Rede in Wörtern? Im Vergleich zu "nicht-genderinklusiven" Begriffen?

mdb_inkl_words %>%
  filter(!is.na(geschlecht)) %>%
  mutate(rel_inkl_words = genderinkl_words/woerter) %>%
  arrange(-rel_inkl_words) %>%
  View("wat")
