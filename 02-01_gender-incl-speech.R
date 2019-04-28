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

mdb_gfl_phrases <- mdb_gfl_speeches %>%                                  # extrahiert alle mit "und" oder "oder" getrennten Wörter
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

mdb_gfl_phrases %>%
  head(10) %>%
  rename(Forumulierung = gfl_phrases) %>%
  knitr::kable(format = "latex", booktabs = TRUE, linesep = "") %>%
  write_file(., "document/tables/genderinklusive_formulierungen_top.tex")

# Auch hier muss die Tabelle wieder manuell angepasst werden, da dies nicht immer funktioniert.
# Tabelle als Beispiel für die zehn häufigst genutzten Genderinklusiven Ansprachen.

gfl_modified_phrases <- read_csv("data/gfl_phrases_modified.csv")

dict_summary_table <- tibble("Genderinklusive Formulierungen" = nrow(gfl_modified_phrases), 
  "Genderinklusive Begriffe" = nrow(genderinkl_begriffe), 
  "Genderexklusive Begriffe" = nrow(genderexkl_begriffe)) 

dict_summary_table %>%
  gather() %>% 
  knitr::kable(format = "latex", booktabs = TRUE, linesep = "",
               col.names = c("", "Anzahl")) %>%
  write_file(., "document/tables/genderinklusive_woerter_anzahl.tex")


## Test der Hypothesen
# Test der Hypothese: Frauen benutzen eher genderinklusive Sprache als Männer.
# Test über: Anteil genderinklusiver Sprache in den Reden von Frauen und Männern. Anteil über Wörter.

# Abschließende Tabelle mit der Anzahl an Begriffen.
genderexkl_begriffe <- gend_dict %>%
  distinct(word_list)

genderinkl_begriffe <- gend_dict %>%
  distinct(alternative_list)


# Diese Auswertung dauert sehr lange. Circa eine Stunde für die Auswertung.
mdb_gfl_speeches <- mdb_full_speeches %>%
  mutate(rede_full = tolower(rede_full)) %>%
  mutate(genderexkl_words = 
           str_extract_all(rede_full,
                           paste(tolower(genderexkl_begriffe$word_list), collapse = "|"))) %>%
  mutate(genderinkl_words = 
           str_extract_all(rede_full,
                           paste(tolower(genderinkl_begriffe$alternative_list), collapse = "|"))) %>%
  mutate(gender_phrases = 
           str_extract_all(rede_full,
                           paste(gfl_modified_phrases$gfl_phrases, collapse = "|")))

write_rds(mdb_gfl_speeches, "data/BT_19/gfl_speeches.RDS")
# mdb_gfl_speeches <- read_rds("data/BT_19/gfl_speeches.RDS")

# Nachdem wir alle inklusiven, exklusiven Begriffe und inklusiven Ansprachen extrahiert haben, können wir den Anteil der genderinklusiven Ansprache untersuchen.
gfl_total <- mdb_gfl_speeches %>%
  mutate(count_genderinkl_words = lengths(genderinkl_words), count_genderexkl_words = lengths(genderexkl_words),
         count_genderinkl_phrases = lengths(gender_phrases)) %>%
  select(rede_id, geschlecht, woerter, count_genderinkl_words, count_genderinkl_words, count_genderinkl_phrases) %>%
  filter(woerter > 100) %>%      # Reden mit mindestens 100 Wörtern, ansonsten verfälschend, 7843 Reden insgesamt
  mutate(count_genderinkl_total = count_genderinkl_words + count_genderinkl_phrases * 3) %>% # genderinklusive Ansprache besteht immer aus drei Wörtern
  mutate(prop_genderinkl_words = count_genderinkl_total / woerter) %>%
  mutate(prop_genderexkl_words = count_genderinkl_words / woerter) %>%
  arrange(-prop_genderinkl_words)

gfl_total %>%
  ggplot(aes(x = geschlecht, y = prop_genderexkl_words)) +
  geom_boxplot() +
  labs(title = "Anteil geschlechterexklusiver Begriffe und Formulierungen pro Rede",
       subtitle = "Auswertung von 7.843 Reden des 19. Deutschen Bundestages nach Geschlecht",
       y = "Anteil geschlechterexklusiver Begriffe und Formulierungen",
       x = "Geschlecht MdB") +
  theme_minimal()

ggsave("document/images/boxplot_gfl.pdf", device = "pdf", height = 15, width = 18, units = "cm", dpi = 300)

# Abschließend wird ein T-Test durchgeführt um zu untersuchen, ob ein signifikanter 
# Unterschiedzwischen den Gruppen festzustellen ist

# Zunächst: Vergleich der Varianzen über den Levene-Test um die Homogeninität der Varianzen zu überprüfen
leveneTest(prop_genderinkl_words ~ geschlecht, data = gfl_total)
# Varianzen sind inhomogen, deshalb t-test mit var.equal FALSE
t_test(prop_genderinkl_words ~ geschlecht, data = gfl_total, var.equal = FALSE) %>%
  t_apa(., format = "latex", print = FALSE) %>%
  write_file(., "document/results/t-test_apa_result.tex")

xtable(summarize(Orthodont, type = "numeric", group = "Sex",
                 test = c("wilcox.test", "t.test")))

t.test(prop_genderinkl_words ~ geschlecht, data = gfl_total, var.equal = FALSE) -> wat

summarize(wat, type = "numeric", group = "Geschlecht", test = c("t.test")))
