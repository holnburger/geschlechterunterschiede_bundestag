#-------------------#
#       SETUP       #
#-------------------#

library(MASS)
library(tidyverse)
library(stargazer)

# Überprüfung Hypothesen zu Unterbrechungen und Zwischenfragen
# Basisdaten

bt_overview <- read_rds("data/BT_19/overview.RDS")
bt_data <- read_rds("data/mdb_data.RDS")
bt_speeches <- read_rds("data/BT_19/speeches.RDS")
fraktionsvorsitz <- read_rds("data/BT_19/fraktionsvorsitzende.RDS")

# Korrekturdaten
# Carsten Träger und Marja-Lisa Völlers haben teilweise falsche Angaben zur ID in den Protokollen

corr_traeger <- bt_data %>%
  filter(id == "11004426") %>%
  mutate(id = "999190001")

corr_voellers <- bt_data %>%
  filter(id == "11004942") %>%
  mutate(id = "10000")

bt_data <- bt_data %>%
  bind_rows(corr_traeger) %>%
  bind_rows(corr_voellers)

# Ergänzende Daten

parteien <- c("SPD", "DIE LINKE", "FDP", "CDU/CSU", "AfD", "BÜNDNIS 90/DIE GRÜNEN")
parteien_in_comments <- "\\[SPD\\]|\\[DIE LINKE\\]|\\[FDP\\]|\\[CDU/CSU\\]|\\[AfD\\]|\\[BÜNDNIS 90/DIE GRÜNEN\\]"
oppositions_parteien <- c("DIE LINKE", "FDP", "AfD", "BÜNDNIS 90/DIE GRÜNEN")
links_parteien <- c("DIE LINKE", "BÜNDNIS 90/DIE GRÜNEN", "SPD")
negativ_comments <- c("Zuruf", "Lachen", "Zwischenruf", "Gegenruf")

#--------------------#
#  Data-Preperation  #
#--------------------#

## Negative Unterbrechungen (Lachen, Zurufe, Gegenrufe, Widerspruch)

bt_comments <- bt_speeches %>%
  left_join(bt_overview %>% select(rede_id, redner_rolle),
            by = "rede_id") %>%                          # Nur die Reden von MdBs
  filter(is.na(redner_rolle)) %>%                       
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

# Einfache Tabelle mit Dummy-Variablen und relevanten Untersuchungsvariablen

bt_comments_overview <- bt_comments %>%
  filter(neg_kommentar == TRUE) %>%                      # Nur negative Kommentare, keine positiven
  group_by(rede_id) %>%                                  # Gruppieren nach Rede
  summarise(neg_kommentare = n()) %>%                    # Anzahl der negativen Kommentare
  right_join(bt_overview %>%
               filter(is.na(redner_rolle)),
             by = "rede_id") %>%                         # Mit weiteren Angaben (Geschlecht, Partei, etc.) kombinieren
  left_join(bt_data, by = c("redner_id" = "id")) %>%
  mutate(name = paste0(redner_vorname, " ", redner_nachname, ", ", redner_fraktion)) %>% # Lesbare Angaben über Abgeordnete
  select(rede_id, redner_id, name, redner_fraktion,
         neg_kommentare, geburtsjahr, partei,
         geschlecht, anzahl_wahlperioden) %>%
  mutate(neg_kommentare = replace_na(neg_kommentare, 0)) %>%                        # Werte ohne neg Unterbrechung als 0
  mutate(gender = ifelse(geschlecht == "männlich", 1, 0)) %>%                       # Dummy-Variable Geschlecht
  mutate(opposition = ifelse(redner_fraktion %in% oppositions_parteien, 1, 0)) %>%  # Dummy-Varibale Opposition               
  mutate(rechts = ifelse(redner_fraktion %in% links_parteien, 0, 1)) %>%            # Dummy-Variable rechts
  mutate(is_afd = ifelse(redner_fraktion == "AfD", 1, 0)) %>%                       # Dummy-Variable AfD
  mutate(alter = 2018 - geburtsjahr) %>%                                            # Angabe Alter
  select(-geburtsjahr, -geschlecht) %>%
  mutate(vorsitz = ifelse(str_detect(paste(fraktionsvorsitz$name, collapse = "|"), name), 1, 0))  # Dummy-Variable Fraktionsvorsitz

write_rds(bt_comments_overview, "data/BT_19/comments_overview.RDS")

#--------------#
#  Statsitics  #
#--------------#

bt_comments_overview %>% 
  mutate(Geschlecht = ifelse(gender == 0, "weiblich", "männlich")) %>%
  group_by(Geschlecht) %>%
  summarise(min = min(neg_kommentare), 
            max = max(neg_kommentare),
            mean = mean(neg_kommentare),
            sd = sd(neg_kommentare),
            n = n(),
            var = var(neg_kommentare)) %>%
  knitr::kable(format = "latex", booktabs = TRUE) %>%
  write_file(., "results/unterbrechungen_pro_rede.tex")

hist(bt_comments_overview$neg_kommentare, main = "Histogram: Unterbrechungen während \n Bundestagsreden", 
     xlab = "Unterbrechungen")

bt_comments_overview %>% select(gender, neg_kommentare) %>% split(.$gender) %>% map(summary)

#----------------------#
#  Hypotheses Testing  #
#----------------------#

# Negative Binominalregression

m1 <- glm.nb(neg_kommentare ~ gender, data = bt_comments_overview)
m2 <- glm.nb(neg_kommentare ~ gender + opposition, data = bt_comments_overview)
m3 <- glm.nb(neg_kommentare ~ gender + opposition + rechts, data = bt_comments_overview)
m4 <- glm.nb(neg_kommentare ~ gender + opposition + rechts + is_afd, data = bt_comments_overview)
m5 <- glm.nb(neg_kommentare ~ gender + alter, data = bt_comments_overview)
m6 <- glm.nb(neg_kommentare ~ gender + anzahl_wahlperioden, data = bt_comments_overview)
m7 <- glm.nb(neg_kommentare ~ gender + vorsitz, data = bt_comments_overview)

stargazer(m1, m2, m3, m4, m5, m6, m7, type = "latex", omit.stat=c("LL","ser","f"), no.space=TRUE, 
          title = "Negative Binominalregression", dep.var.caption = "Abhängige Variable",
          dep.var.labels = c("Unterbrechungen in Reden"),
          covariate.labels = c("Geschlecht", "Opposition", "Rechts", "AfD", "Alter", "Anzahl Mandate im BT", "Fraktionsvorsitz"),
          out = "results/glm_nb_unterbrechungen.tex")

#--------------------#
#  Data-Preperation  #
#--------------------#

## Zwischenfragen

# Zwei Möglichkeiten: Wir können die Anzahl der Anfragen für Zwischenfragen messen oder die tatsächlichen Zwischenfragen. 
# Wir entscheiden uns für ersteres.

bt_zwischenfragen <- bt_speeches %>%     # Schlägt an, wenn das Präsidium fragen zu Zwischenfrage stellt. 
  mutate(anfrage_zwischenfrage =
           ifelse(präsidium == TRUE & str_detect(rede, "Zwischenfrage"), 1, 0)) %>%
  group_by(rede_id) %>%
  summarise(anzahl_anfrage_zwischenfrage = 
              sum(anfrage_zwischenfrage)) %>%
  arrange(-anzahl_anfrage_zwischenfrage) %>%
  left_join(bt_overview, by = "rede_id") %>%      # Ergänzung um weitere Informationen
  mutate(name = paste0(redner_vorname, " ", redner_nachname, ", ", redner_fraktion)) %>%
  filter(is.na(redner_rolle)) %>%                 # nur Reden von MdBs    
  left_join(bt_data, by = c("redner_id" = "id")) %>%
  mutate(gender = ifelse(geschlecht == "männlich", 1, 0)) %>%
  mutate(alter = 2018 - geburtsjahr) %>%
  select(rede_id, anzahl_anfrage_zwischenfrage, redner_id, name, redner_fraktion, gender, alter, anzahl_wahlperioden) %>%
  mutate(vorsitz = ifelse(str_detect(paste(fraktionsvorsitz$name, collapse = "|"), name), 1, 0)) %>%
  mutate(opposition = ifelse(redner_fraktion %in% oppositions_parteien, 1, 0)) %>%
  mutate(rechts = ifelse(redner_fraktion %in% links_parteien, 0, 1)) %>%
  mutate(is_afd = ifelse(redner_fraktion == "AfD", 1, 0)) %>%
  na.omit(gender)

write_rds(bt_zwischenfragen, "data/BT_19/zwischenfragen_overview.RDS")

#----------------------#
#  Hypotheses Testing  #
#----------------------#

# Negative Binomialregression

m1 <- glm.nb(anzahl_anfrage_zwischenfrage ~ gender, data = bt_zwischenfragen)
m2 <- glm.nb(anzahl_anfrage_zwischenfrage ~ gender + opposition, data = bt_zwischenfragen)
m3 <- glm.nb(anzahl_anfrage_zwischenfrage ~ gender + opposition + rechts, data = bt_zwischenfragen)
m4 <- glm.nb(anzahl_anfrage_zwischenfrage ~ gender + opposition + rechts + is_afd, data = bt_zwischenfragen)
m5 <- glm.nb(anzahl_anfrage_zwischenfrage ~ gender + alter, data = bt_zwischenfragen)
m6 <- glm.nb(anzahl_anfrage_zwischenfrage ~ gender + anzahl_wahlperioden, data = bt_zwischenfragen)
m7 <- glm.nb(anzahl_anfrage_zwischenfrage ~ gender + vorsitz, data = bt_zwischenfragen)


stargazer(m1, m2, m3, m4, m5, m6, m7, type = "latex", omit.stat=c("LL","ser","f"), no.space=TRUE, 
          dep.var.labels = c("Anfragen zu Zwischenfragen während der Rede"), dep.var.caption = "Unabhängige Variable",
          title = "Negative Binomialregression",
          covariate.labels = c("Geschlecht", "Opposition", "Rechts", "AfD", "Alter", "Anzahl Mandate", "Fraktionsvorsitz"),
          out = "results/glm_nb_zwischenfragen.tex")
