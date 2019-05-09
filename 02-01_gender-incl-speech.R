# Setup

library(tidyverse)
library(knitr)
library(kableExtra)
library(car)
library(apa)
library(PMCMRplus) # K-W und Post-Hoc Test
library(onewaytests) # ANOVA
library(multcompView) # Darstellung des Post-Hoc Test
require(scales)
library(xtable)
library(PMCMR)

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
  mutate(prop_gender_total = count_genderinkl_phrases*3+count_genderinkl_words) %>%
  mutate(prop_gender_total = prop_gender_total/woerter) %>%
  select(rede_id, geschlecht, name, redner_fraktion, partei, anzahl_wahlperioden, gender, opposition, rechts, vorsitz, is_afd,
         alter, prop_genderinkl_words, prop_genderexkl_words, prop_gender_phrases, woerter, prop_gender_total)

#### Auswertung

# Boxplot: GFL-Nutzung gegen Geschlecht
gfl_total %>% 
  ggplot(aes(x = as.character(geschlecht), y = prop_gender_total)) +
  geom_boxplot() +
  labs(#title = "Anteil GFL-Begriffe und Formulierungen\npro Rede",
       #subtitle = "Auswertung von 7.843 Reden des\n19. Deutschen Bundestages nach Geschlecht",
       y = "Anteil geschlechterinklusiver Begriffe und Formulierungen",
       x = "Geschlecht MdB") +
  theme_minimal()

ggsave("document/images/boxplot_gfl.pdf", device = "pdf", height = 15, width = 10, units = "cm", dpi = 300)

gfl_total %>% 
  mutate(redner_fraktion = ifelse(redner_fraktion == "BÜNDNIS 90/DIE GRÜNEN", "B90/\nDIE GRÜNEN", redner_fraktion)) %>%
  mutate(redner_fraktion = ifelse(redner_fraktion == "CDU/CSU", "CDU/\nCSU", redner_fraktion)) %>%
  ggplot(aes(x = as.character(redner_fraktion), y = prop_gender_total)) +
  geom_boxplot() +
  labs(#title = "Anteil GFL-Begriffe und Formulierungen\npro Rede",
    #subtitle = "Auswertung von 7.843 Reden des\n19. Deutschen Bundestages nach Geschlecht",
    y = "Anteil geschlechterinklusiver Begriffe und Formulierungen",
    x = "Fraktion") +
  theme_minimal()

ggsave("document/images/boxplot_gfl_fraktion.pdf", device = "pdf", height = 15, width = 10, units = "cm", dpi = 300)

## Test der Hypothesen
# Test der Hypothese: Frauen benutzen eher genderinklusive Sprache als Männer.
# Test über: Anteil genderinklusiver Sprache in den Reden von Frauen und Männern. Anteil über Wörter.

# Abschließend wird ein T-Test durchgeführt um zu untersuchen, ob ein signifikanter 
# Unterschiedzwischen den Gruppen festzustellen ist

# Zunächst: Vergleich der Varianzen über den Levene-Test um die Homogeninität der Varianzen zu überprüfen
leveneTest(prop_gender_total ~ geschlecht, data = gfl_total)
# Varianzen sind inhomogen, deshalb t-test mit var.equal FALSE
t_test(prop_gender_phrases ~ fct_relevel(gfl_total$geschlecht, "weiblich"), 
       data = gfl_total, var.equal = FALSE, alternative = c("greater")) %>%
  t_apa(., format = "latex", print = FALSE) %>%
  write_file(., "document/results/t-test_apa_result.tex")

## Untersuchung der unterschiede zwischen den Parteien

## ANOVA 
res_anova <- aov(prop_gender_total ~ redner_fraktion, data = gfl_total)
qqnorm(res_anova$residuals)
summary(res_anova)

# Signifikanter Unterschied zwischen den Parteien.
xtable(summary.lm(res_anova), type = "latex")

# Keine Normalverteilung, prüfen von Homoskedastizität - erst in Faktoren umwandeln
gfl_anova_test <- gfl_total %>% ungroup() %>% mutate(redner_fraktion = as.factor(redner_fraktion))
homog.test(prop_gender_total ~ redner_fraktion, data = gfl_anova_test)
# Auch keine Homoskedastiziät

# Lösung: Nonparametrische Varianzanalyse mit dem Kruskal-Wallis test

kruskal.test(prop_gender_total ~ redner_fraktion, data = gfl_anova_test)

# Kruskal-Wallis test ist signifkant mit p < 0.01
# Tabelle erstellen mit Post-Hoc Bonferroni-Dunn-Test

out <- dunnettT3Test(x = gfl_anova_test$prop_gender_total, g=gfl_anova_test$redner_fraktion, dist="bonf")
out <- posthoc.kruskal.dunn.test(x = gfl_anova_test$prop_gender_total, g=gfl_anova_test$redner_fraktion, dist="bonf") 
out.p <- get.pvalues(out) 
out.mcV <- multcompLetters(out.p, threshold=0.05) 
Rij <- rank(gfl_anova_test$prop_gender_total) 
Rj.mean <- tapply(Rij, gfl_anova_test$redner_fraktion, mean) 
dat <- data.frame(Group = names(Rj.mean), 
                  meanRj = Rj.mean, 
                  M = out.mcV$Letters) 
dat.x <- xtable(dat) 
caption(dat.x) <- c("Post-Hoc Bonferroni-Dunn-Test. Unterschiedliche Buchstaben (M) 
weisen auf signifikante Unterschiede ($p < 0.05$) zwischen den Fraktionen hin.", "Post-Hoc Bonferroni-Dunn-Test") 
colnames(dat.x) <- c("Fraktion", "$\\bar{R}_{j}$", "M")
digits(dat.x) <- 1
label(dat.x) <- "table:dunn-test"
print(dat.x, include.rownames = F, file = "document/tables/dunn-test.tex", sanitize.text.function=function(x){x})
