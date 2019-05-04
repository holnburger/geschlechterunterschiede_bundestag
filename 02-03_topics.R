library(tidyverse)
library(stm)
library(furrr)
plan(multiprocess)


# Laden der Dateien
mdb_data <- read_rds("data/mdb_data.RDS")
overview <- read_rds("data/BT_19/overview.RDS")
speeches <- read_rds("data/BT_19/speeches.RDS")
full_speeches <- read_rds("data/BT_19/full_speeches.RDS")

parteien <- c("SPD", "DIE LINKE", "FDP", "CDU/CSU", "AfD", "BÜNDNIS 90/DIE GRÜNEN")
fraktionsvorsitz <- read_rds("data/BT_19/fraktionsvorsitzende.RDS")
oppositions_parteien <- c("DIE LINKE", "FDP", "AfD", "BÜNDNIS 90/DIE GRÜNEN")
links_parteien <- c("DIE LINKE", "BÜNDNIS 90/DIE GRÜNEN", "SPD")

# Redebeiträge des 19. Bundestag, nur von Abgeordneten, ohne Unterbrechungen und Zwischenfragen
speeches_clean <- full_speeches %>%
  filter(woerter > 100) %>%
  filter(redner_fraktion %in% parteien) %>%
  mutate(name = paste0(redner_vorname, " ", redner_nachname, ", ", redner_fraktion)) %>%
  select(-redner_vorname, -redner_nachname) %>%
  left_join(mdb_data %>% select(id, partei, geburtsjahr, anzahl_wahlperioden), by = c("redner_id" = "id")) %>%
  mutate(vorsitz = ifelse(str_detect(paste(fraktionsvorsitz$name, collapse = "|"), name), 1, 0)) %>%  # Dummy-Variable Fraktionsvorsitz 
  mutate(gender = ifelse(geschlecht == "männlich", 1, 0)) %>%                       # Dummy-Variable Geschlecht
  mutate(opposition = ifelse(redner_fraktion %in% oppositions_parteien, 1, 0)) %>%  # Dummy-Varibale Opposition               
  mutate(rechts = ifelse(redner_fraktion %in% links_parteien, 0, 1)) %>%            # Dummy-Variable rechts
  mutate(is_afd = ifelse(redner_fraktion == "AfD", 1, 0)) %>%                       # Dummy-Variable AfD
  mutate(alter = 2018 - geburtsjahr) %>%
  select(-wahlperiode, - geburtsjahr) 

write_rds(speeches_clean, "data/stm/speeches_clean.rds")

# Structural Topic Modeling

# Wir definieren zunächst eine Anzahl an Stopwörtern, welche uns aufgrund der Häufigkeit nicht interessieren.

bt_stopwords = c("bundestag", "kolleginnen", "kollegen", "präsident",
                 "haben", "wollen", "müssen", "mehr", "wurden",
                 "unser", "herren", "damen", "bundesregierung", "frau",
                 "herr", "dass", "deutschland", "schon")

processed <- textProcessor(speeches_clean$rede_full,
                           removestopwords = TRUE,
                           stem = FALSE,
                           metadata=speeches_clean,
                           customstopwords = bt_stopwords,
                           language = "german")

out <- prepDocuments(processed$documents, processed$vocab, processed$meta)

set.seed(123)
stm_speeches_fit <- stm(documents = out$documents, vocab = out$vocab, K = 0,
                        prevalence = ~geschlecht, max.em.its = 500, data = out$meta,
                        init.type = "Spectral")

stm_speeches_fit <- read_rds("data/stm/stm_speeches_fit.RDS")

# Labeling der Topics, export der Wörter pro Topi

capture.output(labelTopics(stm_speeches_fit, n = 10), file = "results/stm_topics.txt")

# Geschlechterunterschiede im Bundestag

out$meta$geschlecht <- as.factor(out$meta$geschlecht)
prep <- estimateEffect(~ geschlecht, stm_speeches_fit,
                       meta = out$meta, uncertainty = "Global")

# Beispiele für Wörter in einem Topic

plot(stm_speeches_fit, topics = c(49, 51, 4, 9, 11), type = "labels", labeltype = "frex")

plot(prep, covariate = "geschlecht", topics = c(35, 51, 4, 9, 11),
     model = stm_speeches_fit, method = "difference",
     xlim = c(-.1, .1), cov.value1 = "weiblich", cov.value2 = "männlich",
     main = "Geschlechterspezifische Effekte zu den Topics im Bundestag",
     custom.labels = c("Schwangerschaftsabbruch", "Klimaschutz", "Grenzschutz", "Brexit", "Bildungspolitik"), labeltype = "custom",
     xlab = "Eher von Männern behandelt ... Eher von Frauen behandelt")

findThoughts(stm_speeches_fit, texts = speeches_clean$rede_full, topics = 4)

plot(prep, topics = c(1:20), model = stm_speeches, method = "labels")
