#-------------------#
#       SETUP       #
#-------------------#

library(tidyverse)
library(knitr)
library(kableExtra)
library(car) # leven Test
library(apa) # T-Test
library(onewaytests) # Homog. Test
library(PMCMR)
library(PMCMRplus)
library(multcompView)
library(xtable)

# Überprüfung Hypothesen zu Unterbrechungen und Zwischenfragen
# Basisdaten

# Laden der Dateien
mdb_data <- read_rds("data/mdb_data.RDS")
overview <- read_rds("data/BT_19/overview.RDS")
speeches <- read_rds("data/BT_19/speeches.RDS")
full_speeches <- read_rds("data/BT_19/full_speeches.RDS") %>%
  select(rede_id, woerter, geschlecht)

parteien <- c("SPD", "DIE LINKE", "FDP", "CDU/CSU", "AfD", "BÜNDNIS 90/DIE GRÜNEN")
parteien_in_comments <- "\\[SPD\\]|\\[DIE LINKE\\]|\\[FDP\\]|\\[CDU/CSU\\]|\\[AfD\\]|\\[BÜNDNIS 90/DIE GRÜNEN\\]"
fraktionsvorsitz <- read_csv("data/BT_19/fraktionsvorsitzende_o_stellv.csv")
oppositions_parteien <- c("DIE LINKE", "FDP", "AfD", "BÜNDNIS 90/DIE GRÜNEN")
links_parteien <- c("DIE LINKE", "BÜNDNIS 90/DIE GRÜNEN", "SPD")

negativ_comments <- c("Zuruf", "Lachen", "Zwischenruf", "Gegenruf")

## Negative Unterbrechungen (Lachen, Zurufe, Gegenrufe, Widerspruch)

mdb_comments <- speeches %>%
  left_join(overview %>% select(rede_id, redner_rolle),
            by = "rede_id") %>%                          # Nur die Reden von MdBs
  filter(fraktion %in% parteien) %>%
  filter(typ == "kommentar") %>%                         # Nur die Kommentare
  separate_rows(rede, sep = "–") %>%                     # Kommentare bei Bindestrich in die nächst Datenreihe
  mutate(rede = str_remove_all(rede, "^\\s+|\\s+$")) %>% # Entfernt Leerzeichen vor und nach dem String
  mutate(rede = str_remove_all(rede, "^\\(|\\)$")) %>%   # Entfernt Klammern vor und nach dem String
  filter(fraktion %in% parteien) %>%                     # Nur die Fraktionen, keine fraktionslosen Abgeordneten
  filter(präsidium != TRUE) %>%                          # Keine Kommentare des Präsidiums
  mutate(neg_kommentar = case_when(                      # Erkennt Zwischenrufe und negative Unterbrechungen
    str_detect(rede, parteien_in_comments) ~ TRUE,
    str_detect(rede, paste(negativ_comments, collapse = "|")) ~ TRUE,
    TRUE ~ FALSE))

mdb_comments_overview <- mdb_comments %>%
  filter(neg_kommentar == TRUE) %>%                      # Nur negative Kommentare, keine positiven
  group_by(rede_id) %>%                                  # Gruppieren nach Rede
  summarise(neg_kommentare = n()) %>%                    # Anzahl der negativen Kommentare
  right_join(overview %>% filter(redner_fraktion %in% parteien), by = "rede_id") %>%  # Mit weiteren Angaben (Geschlecht, Partei, etc.) kombinieren
  left_join(mdb_data %>% select(-geschlecht), by = c("redner_id" = "id")) %>%
  left_join(full_speeches, by = "rede_id") %>%
  mutate(name = paste0(redner_vorname, " ", redner_nachname, ", ", redner_fraktion)) %>% # Lesbare Angaben über Abgeordnete
  select(rede_id, redner_id, name, redner_fraktion,
         neg_kommentare, geburtsjahr, partei,
         geschlecht, anzahl_wahlperioden, woerter) %>%                                       
  filter(woerter > 100) %>%                                                         # Nur Reden mit mehr als 100 Wörtern
  mutate(neg_kommentare = replace_na(neg_kommentare, 0)) %>%                        # Werte ohne neg Unterbrechung als 0
  mutate(gender = ifelse(geschlecht == "männlich", 1, 0)) %>%                       # Dummy-Variable Geschlecht
  mutate(opposition = ifelse(redner_fraktion %in% oppositions_parteien, 1, 0)) %>%  # Dummy-Varibale Opposition               
  mutate(rechts = ifelse(redner_fraktion %in% links_parteien, 0, 1)) %>%            # Dummy-Variable rechts
  mutate(is_afd = ifelse(redner_fraktion == "AfD", 1, 0)) %>%                       # Dummy-Variable AfD
  mutate(alter = 2018 - geburtsjahr) %>%                                            # Angabe Alter
  select(-geburtsjahr) %>%
  mutate(prop_neg_kommentare = neg_kommentare/woerter) %>%                          # Anteilige Unterbrechungen nach Länge der Rede
  mutate(vorsitz = ifelse(str_detect(paste(fraktionsvorsitz$name, collapse = "|"), name), 1, 0))  # Dummy-Variable Fraktionsvorsitz

# Datensatz ist jetzt vollständig

write_rds(mdb_comments_overview, "data/BT_19/comments_overview.RDS")

#------------#
# Auswertung #
#------------#

# Grafiken: Histogramme für die Länge der Reden in Wörtern und Anzahl der Unterbrechungen
mdb_comments_overview %>%
  ggplot(aes(woerter)) +
  geom_histogram(binwidth = 30) +
  labs(title = "Länge der Reden in Wörtern",
       subtitle = "Auswertung von 7.843 Reden des Bundestages mit mehr als 100 Wörtern",
       y = "Anzahl der Reden",
       x = "Anzahl der Wörter") +
  theme_minimal()
ggsave("document/images/histo_laenge_rede.pdf", device = "pdf", height = 8, width = 15, units = "cm", dpi = 300)


mdb_comments_overview %>%
  ggplot(aes(neg_kommentare)) +
  geom_histogram(binwidth = 1) +
  labs(title = "Negative Unterbrechungen pro Reden Bundestagsrede",
       subtitle = "Auswertung von 7.843 Reden des Bundestages mit mehr als 100 Wörtern",
       y = "Anzahl der Reden",
       x = "Negative Unterbrechungen") +
  theme_minimal()
ggsave("document/images/histo_anzahl_unterbrechung.pdf", device = "pdf", height = 8, width = 15, units = "cm", dpi = 300)


#--------------#
#  Statsitics  #
#--------------#

mdb_comments_overview %>% 
  group_by(geschlecht) %>%
  summarise(min = min(neg_kommentare), 
            max = max(neg_kommentare),
            mean = mean(neg_kommentare),
            sd = sd(neg_kommentare),
            n = n(),
            var = var(neg_kommentare)) %>%
  knitr::kable(format = "latex", booktabs = TRUE) %>%
  write_file(., "results/unterbrechungen_pro_rede.tex")

## Auswertung

# Unterbrechungen nach Geschlecht
# Boxplot
mdb_comments_overview %>%
  ggplot(aes(x = geschlecht, y = prop_neg_kommentare)) +
  geom_boxplot() +
  labs(#title = "Anteil GFL-Begriffe und Formulierungen\npro Rede",
    #subtitle = "Auswertung von 7.843 Reden des\n19. Deutschen Bundestages nach Geschlecht",
    y = "Anteil Unterbrechungen pro Rede",
    x = "Geschlecht MdB") +
  theme_minimal()

ggsave("document/images/boxplot_unterbrechung_geschlecht.pdf", device = "pdf", height = 15, width = 10, units = "cm", dpi = 300)

# Unterbrechnung nach Fraktionsvorsitz
mdb_comments_overview %>%
  mutate(vorsitz = ifelse(vorsitz == 1, "Fraktionsvorsitz", "Kein Vorsitz")) %>%
  ggplot(aes(x = vorsitz, y = prop_neg_kommentare)) +
  geom_boxplot() +
  labs(#title = "Anteil GFL-Begriffe und Formulierungen\npro Rede",
    #subtitle = "Auswertung von 7.843 Reden des\n19. Deutschen Bundestages nach Geschlecht",
    y = "Anteil Unterbrechungen pro Rede",
    x = "Status MdB") +
  theme_minimal()

ggsave("document/images/boxplot_unterbrechung_vorsitz.pdf", device = "pdf", height = 15, width = 10, units = "cm", dpi = 300)

# Unterbrechungen nach Fraktionen
mdb_comments_overview %>%
  mutate(redner_fraktion = ifelse(redner_fraktion == "BÜNDNIS 90/DIE GRÜNEN", "BÜNDNIS 90/\nDIE GRÜNEN", redner_fraktion)) %>%
  ggplot(aes(x = redner_fraktion, y = prop_neg_kommentare)) +
  geom_boxplot() +
  labs(#title = "Anteil GFL-Begriffe und Formulierungen\npro Rede",
    #subtitle = "Auswertung von 7.843 Reden des\n19. Deutschen Bundestages nach Geschlecht",
    y = "Anteil Unterbrechungen pro Rede",
    x = "Fraktion") +
  theme_minimal()

ggsave("document/images/boxplot_unterbrechung_fraktion.pdf", device = "pdf", height = 15, width = 13, units = "cm", dpi = 300)

# Test: Unterbrechung Geschlecht ohne AfD
mdb_comments_overview %>%
  filter(is_afd == 0) %>%
  ggplot(aes(x = geschlecht, y = prop_neg_kommentare)) +
  geom_boxplot() +
  labs(#title = "Anteil GFL-Begriffe und Formulierungen\npro Rede",
    #subtitle = "Auswertung von 7.843 Reden des\n19. Deutschen Bundestages nach Geschlecht",
    y = "Anteil Unterbrechungen pro Rede",
    x = "Geschlecht MdB") +
  theme_minimal()

## Test der Hypothesen
# Test der Hypothese: Frauen werden eher unterbrochen als Männer.

# Abschließend wird ein T-Test durchgeführt um zu untersuchen, ob ein signifikanter 
# Unterschiedzwischen den Gruppen festzustellen ist

# Zunächst: Vergleich der Varianzen über den Levene-Test um die Homogeninität der Varianzen zu überprüfen
leveneTest(prop_neg_kommentare ~ geschlecht, data = mdb_comments_overview)
# Varianzen sind inhomogen, deshalb t-test mit var.equal FALSE
t_test(prop_neg_kommentare ~ fct_relevel(mdb_comments_overview$geschlecht, "weiblich"), 
       data = mdb_comments_overview, var.equal = FALSE, alternative = c("greater")) %>%
  t_apa(., format = "latex", print = FALSE) %>%
  write_file(., "document/results/t-test_apa_result_unterbrechung.tex")

# Test des Fraktionsvorsitzes
leveneTest(prop_neg_kommentare ~ as.character(vorsitz), data = mdb_comments_overview)
# Varianzen sind homogen, deshalb t-test mit var.equal TRUE
t_test(prop_neg_kommentare ~ fct_relevel(recode(mdb_comments_overview$vorsitz, " 1 = 'Vorsitz'; 0 = 'kein Vorsitz'"), "Vorsitz"), 
       data = mdb_comments_overview, var.equal = FALSE, alternative = c("greater")) %>%
  t_apa(., format = "latex", print = FALSE) %>%
  write_file(., "document/results/t-test_apa_result_unterbrechung.tex")

## Überprüfung der Fraktionen
## ANOVA 
res_anova <- aov(prop_neg_kommentare ~ redner_fraktion, data = mdb_comments_overview)
qqnorm(res_anova$residuals)
summary(res_anova)

# Signifikanter Unterschied zwischen den Parteien.
xtable(summary.lm(res_anova), type = "latex")

# Keine Normalverteilung, prüfen von Homoskedastizität - erst in Faktoren umwandeln
unt_anova_test <- mdb_comments_overview %>% ungroup() %>% mutate(redner_fraktion = as.factor(redner_fraktion))
homog.test(prop_neg_kommentare ~ redner_fraktion, data = unt_anova_test)
# Auch keine Homoskedastiziät

# Lösung: Nonparametrische Varianzanalyse mit dem Kruskal-Wallis test

kruskal.test(prop_neg_kommentare ~ redner_fraktion, data = unt_anova_test)

# Kruskal-Wallis test ist signifkant mit p < 0.01
# Tabelle erstellen mit Post-Hoc Bonferroni-Dunn-Test

out <- dunnettT3Test(x = unt_anova_test$prop_neg_kommentare, g=unt_anova_test$redner_fraktion, dist="bonf")
out <- posthoc.kruskal.dunn.test(x = unt_anova_test$prop_neg_kommentare, g=unt_anova_test$redner_fraktion, dist="bonf") 
out.p <- get.pvalues(out) 
out.mcV <- multcompLetters(out.p, threshold=0.05) 
Rij <- rank(unt_anova_test$prop_neg_kommentare) 
Rj.mean <- tapply(Rij, unt_anova_test$redner_fraktion, mean) 
dat <- data.frame(Group = names(Rj.mean), 
                  meanRj = Rj.mean, 
                  M = out.mcV$Letters) 
dat.x <- xtable(dat) 
caption(dat.x) <- c("Post-Hoc Bonferroni-Dunn-Test. Unterschiedliche Buchstaben (M) 
weisen auf signifikante Unterschiede ($p < 0.05$) bezüglich der Unterbrechungen der jeweiligen Fraktionen hin.", "Post-Hoc Bonferroni-Dunn-Test Unterbrechungen") 
colnames(dat.x) <- c("Fraktion", "$\\bar{R}_{j}$", "M")
digits(dat.x) <- 1
label(dat.x) <- "table:dunn-test-unterbrechung"
print(dat.x, include.rownames = F, file = "document/tables/dunn-test-unterbrechungen.tex", sanitize.text.function=function(x){x})



