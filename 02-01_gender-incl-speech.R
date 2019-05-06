# Setup

library(tidyverse)
library(car)
library(apa)
library(knitr)
library(kableExtra)

# Daten laden
mdb_overview <- read_rds("data/BT_19/overview.RDS")
mdb_data <- read_rds("data/mdb_data.RDS")
mdb_full_speeches <- read_rds("data/BT_19/full_speeches.RDS")

# Gender-Dictionär laden mit den genderinklusvien und genderexklusiven Begriffen
gender_dict <- read_csv("data/gender_dict_modified.csv")
gender_phrases <- read_csv("data/gfl_phrases_modified.csv")


# Kontrollvariablen
fraktionsvorsitz <- read_rds("data/BT_19/fraktionsvorsitzende.RDS")
oppositions_parteien <- c("DIE LINKE", "FDP", "AfD", "BÜNDNIS 90/DIE GRÜNEN")
links_parteien <- c("DIE LINKE", "BÜNDNIS 90/DIE GRÜNEN", "SPD")

parteien <- c("SPD", "DIE LINKE", "FDP", "CDU/CSU", "AfD", "BÜNDNIS 90/DIE GRÜNEN")

# Vorbereitung der Daten
mdb_full_speeches <- mdb_full_speeches %>%
  filter(redner_fraktion %in% parteien) %>%
  left_join(mdb_data %>% select(-geschlecht), by = c("redner_id" = "id")) %>%
  mutate(name = paste0(redner_vorname, " ", redner_nachname, ", ", redner_fraktion)) %>%
  select(rede_id, redner_id, name, redner_fraktion, geburtsjahr, partei,
         geschlecht, anzahl_wahlperioden, rede_full, woerter) %>%
  mutate(gender = ifelse(geschlecht == "männlich", 1, 0)) %>%                       # Dummy-Variable Geschlecht
  mutate(opposition = ifelse(redner_fraktion %in% oppositions_parteien, 1, 0)) %>%  # Dummy-Varibale Opposition               
  mutate(rechts = ifelse(redner_fraktion %in% links_parteien, 0, 1)) %>%            # Dummy-Variable rechts
  mutate(is_afd = ifelse(redner_fraktion == "AfD", 1, 0)) %>%                       # Dummy-Variable AfD
  mutate(alter = 2018 - geburtsjahr) %>%                                            # Angabe Alter
  mutate(vorsitz = ifelse(str_detect(paste(fraktionsvorsitz$name, collapse = "|"), name), 1, 0)) %>%
  select(-geburtsjahr) %>%
  filter(woerter > 100)

# Untersuchung der Anzahl genderinklusiver Formulierungen und Begriffe
# Diese Auswertung dauert sehr lange. Circa eine Stunde für die Auswertung.
mdb_gfl_speeches <- mdb_full_speeches %>%
  mutate(rede_full = tolower(rede_full)) %>%
  mutate(genderexkl_words = 
           str_extract_all(rede_full,
                           paste(gender_dict %>% distinct(word_list) %>% pull() %>% tolower(), collapse = "|"))) %>%
  mutate(genderinkl_words = 
           str_extract_all(rede_full,
                           paste(gender_dict %>% distinct(alternative_list) %>% pull() %>% tolower(), collapse = "|"))) %>%
  mutate(gender_phrases = 
           str_extract_all(rede_full,
                           paste(gender_phrases$gfl_phrases, collapse = "|")))

write_rds(mdb_gfl_speeches, "data/BT_19/gfl_speeches.RDS")
mdb_gfl_speeches <- read_rds("data/BT_19/gfl_speeches.RDS")

# Tabelle mit den häufigsten genderinklusiven Formulierungen

mdb_gfl_speeches %>%
  ungroup() %>%
  unnest(gender_phrases) %>%
  count(gender_phrases, sort = TRUE) %>%
  head(10) %>%
  rename("Genderinklusive Formulierungen" = gender_phrases) %>%
  mutate(n = scales::comma(n, big.mark = ".", decimal.mark = ",")) %>%
  knitr::kable(format = "latex", booktabs = TRUE, linesep = "") %>%
  write_file(., "document/tables/genderinklusive_formulierungen_top.tex")

# Tabelle mit den häufigsten genderinklusiven Begriffen
mdb_gfl_speeches %>%
  ungroup() %>%
  unnest(genderinkl_words) %>%
  count(genderinkl_words, sort = TRUE) %>%
  head(10) %>%
  rename("Genderinklusive Begriffe" = genderinkl_words) %>%
  mutate(n = scales::comma(n, big.mark = ".", decimal.mark = ",")) %>%
  knitr::kable(format = "latex", booktabs = TRUE, linesep = "") %>%
  write_file(., "document/tables/genderinklusive_begriffe_top.tex")

# Untersuchung der Anteile von genderinklusiven und genderexklusiven Begriffen und Formulierungen in den Reden
gfl_total <- mdb_gfl_speeches %>%
  mutate(count_genderinkl_words = lengths(genderinkl_words), count_genderexkl_words = lengths(genderexkl_words),
         count_genderinkl_phrases = lengths(gender_phrases)) %>%
  mutate(count_genderinkl_total = count_genderinkl_words + count_genderinkl_phrases * 3) %>% # genderinklusive Ansprache besteht immer aus drei Wörtern
  mutate(prop_genderinkl_words = count_genderinkl_total / woerter) %>%
  mutate(prop_genderexkl_words = count_genderinkl_words / woerter) %>%
  arrange(-prop_genderinkl_words) %>%
  mutate(prop_gender_phrases = count_genderinkl_phrases*3 / woerter) %>%
  select(rede_id, geschlecht, name, redner_fraktion, partei, anzahl_wahlperioden, gender, opposition, rechts, vorsitz, is_afd,
         alter, prop_genderinkl_words, prop_genderexkl_words, prop_gender_phrases, woerter) %>%
  filter(prop_gender_phrases > 0.05)


#### Auswertung

# Boxplot: GFL-Nutzung gegen Geschlecht
gfl_total %>% 
  ggplot(aes(x = as.character(geschlecht), y = prop_gender_phrases)) +
  geom_boxplot() +
  labs(title = "Anteil geschlechterexklusiver Begriffe und Formulierungen pro Rede",
       subtitle = "Auswertung von 7.843 Reden des 19. Deutschen Bundestages nach Geschlecht",
       y = "Anteil geschlechterexklusiver Begriffe und Formulierungen",
       x = "Geschlecht MdB") +
  theme_minimal()

ggsave("document/images/boxplot_gfl.pdf", device = "pdf", height = 15, width = 18, units = "cm", dpi = 300)



## Test der Hypothesen
# Test der Hypothese: Frauen benutzen eher genderinklusive Sprache als Männer.
# Test über: Anteil genderinklusiver Sprache in den Reden von Frauen und Männern. Anteil über Wörter.

# Abschließend wird ein T-Test durchgeführt um zu untersuchen, ob ein signifikanter 
# Unterschiedzwischen den Gruppen festzustellen ist

# Zunächst: Vergleich der Varianzen über den Levene-Test um die Homogeninität der Varianzen zu überprüfen
leveneTest(prop_gender_phrases ~ geschlecht, data = gfl_total)
# Varianzen sind inhomogen, deshalb t-test mit var.equal FALSE
t_test(prop_gender_phrases ~ geschlecht, data = gfl_total, var.equal = FALSE) 

%>%
  t_apa(., format = "latex", print = FALSE) %>%
  write_file(., "document/results/t-test_apa_result.tex")

xtable(summarize(Orthodont, type = "numeric", group = "Sex",
                 test = c("wilcox.test", "t.test")))

t.test(prop_genderinkl_words ~ geschlecht, data = gfl_total, var.equal = FALSE) -> wat

summarize(wat, type = "numeric", group = "Geschlecht", test = c("t.test")))



