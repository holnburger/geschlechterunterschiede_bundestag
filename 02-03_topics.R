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

# Redebeiträge des 19. Bundestag, nur von Abgeordneten, ohne Unterbrechungen und Zwischenfragen

speeches_clean <- speeches %>%
  filter(fraktion %in% parteien) %>%
  filter(typ == "p") %>%
  filter(präsidium == FALSE) %>%
  group_by(rede_id) %>%
  mutate(rede = paste0(rede, collapse = "\n")) %>%
  distinct(rede, .keep_all = TRUE) %>%
  mutate(rede = str_remove_all(rede, "„|“")) %>%
  mutate(name = paste0(vorname, " ", nachname, ", ", fraktion)) %>%
  select(-vorname, -nachname, -präsidium, -typ, -status) %>%
  left_join(mdb_data %>% select(id, geschlecht, partei, geburtsjahr, anzahl_wahlperioden), by = "id")

write_rds(speeches_clean, "data/stm/speeches_clean.rds")

# Structural Topic Modeling

# Wir definieren zunächst eine Anzahl an Stopwörtern, welche uns aufgrund der Häufigkeit nicht interessieren.

bt_stopwords = c("bundestag", "kolleginnen", "kollegen", "präsident",
                 "haben", "wollen", "müssen", "mehr", "wurden",
                 "unser", "herren", "damen", "bundesregierung", "frau",
                 "herr", "dass", "deutschland", "schon")

processed <- textProcessor(speeches_clean$rede,
                           removestopwords = TRUE,
                           stem = TRUE,
                           metadata=speeches_clean,
                           customstopwords = bt_stopwords,
                           language = "german")

out <- prepDocuments(processed$documents, processed$vocab, processed$meta)

# Wir überprüfen nun, wie viele Topics wir in unserem Datensatz haben.

K <- c(60, 70, 80, 90, 100, 110, 120)

test_k <- searchK(out$documents, out$vocab, K, cores = 6)

plot(test_k)

write_rds(test_k, "data/stm/test_k_multi.RDS")
# Mit einer groben Bestimmung landen wir zwischen 100 und 120 Topics

K_second <- c(100, 110, 120, 130, 140)

test_k_second <- searchK(out$documents, out$vocab, K_second, cores = 6)

plot(test_k_second)

write_rds(test_k_fine, "data/stm/test_k_fine.RDS")

test_k_fine <- read_rds("data/stm/test_k_fine.RDS")

# Den besten fit haben wir bei etwa 110 Topics, dies muss im weiteren Verlauf noch verbessert werden

stm_speeches_fit <- stm(documents = out$documents, vocab = out$vocab, K = 0,
                        prevalence = ~geschlecht, max.em.its = 500, data = out$meta,
                        init.type = "Spectral")

write_rds(stm_speeches_fit, "data/stm/stm_speeches_fit.RDS")

stm_speeches <- read_rds("data/stm/stm_speeches_fit.RDS")

# Überprüfen einiger Topics

out$meta$geschlecht <- as.factor(out$meta$geschlecht)

prep <- estimateEffect(~ geschlecht, stm_speeches,
                       meta = out$meta, uncertainty = "Global")

plot(stm_speeches_fit, topics = 1, type = "labels", labeltype = "frex")
plot(stm_speeches_fit, topics = 1, type = "labels", n = 30)
labelTopics(stm_speeches_fit, topics = c(1:10), n = 10)

plot(prep, covariate = "geschlecht", topics = c(31:40),
     model = stm_speeches, method = "difference",
     xlim = c(-.1, .1), cov.value1 = "weiblich", cov.value2 = "männlich",
     main = "Geschlechterspezifische Effekte zu den Topics im Bundestag",
     #custom.labels = c("Ehe für Alle", "Schwangerschaftsabbruch", "Brexit"), labeltype = "custom",
     xlab = "Eher von Männern behandelt ... Eher von Frauen behandelt")

findThoughts(stm_speeches, texts = speeches_clean, topics = 28)

plot(prep, topics = c(1:20), model = stm_speeches, method = "labels")
