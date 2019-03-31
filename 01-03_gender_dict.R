library(rvest)
library(tidyverse)

site <- read_html("https://geschicktgendern.de/")

word_list <- site %>% 
  html_nodes(".column-1") %>%
  html_text()

alternative_list <- site %>%
  html_nodes(".column-2") %>% 
  html_text()


missing_entry <- "noch kein passender Begriff gefunden"

gender_dict <- tibble(word_list, alternative_list) %>%
  filter(!str_detect(alternative_list, missing_entry)) %>% # Fehlende Einträge raus
  mutate(word_list = str_remove_all(word_list, "\\(.*\\)")) %>% # Alles in Klammern löschen
  mutate(alternative_list = str_remove_all(alternative_list, "\\(.*\\)")) %>%
  mutate(word_list = str_remove_all(word_list, "\\[.*\\]")) %>% # Alles in eckigen Klammern löschen
  mutate(alternative_list = str_remove_all(alternative_list, "\\[.*\\]")) %>%
  separate_rows(alternative_list, sep = ";") %>% # Bei ; neue Zeile
  mutate(word_list = trimws(word_list), alternative_list = trimws(alternative_list)) %>% # Leerzeichen vorher und nachher weg
  mutate(word_list = str_replace_all(word_list, "\\s\\s", "\\s")) %>% # Doppelte Leerzeichen raus
  mutate(alternative_list = str_replace_all(alternative_list, "\\s\\s", "\\s")) %>%
  filter(alternative_list != "") %>% # Leere Zeilen raus
  slice(-1) # erste Reihe löschen (Überschriften)

# Problematisch: Inhaltlich ist das Lexikon noch sehr ungenügend
# Entsprechend wird das Diktionär noch inhaltlich geändert.

# Beispiel: ... Wenn man durchs Fenster sieht, kann man das Meer sehen.	... Wenn Sie durch das Fenster blicken, können Sie das Meer sehen.
# Sehr explizite Sätze, welche wir nicht in den BT-Reden finden werden. Viele Beispiele
# Lösung: Wir filtern alles auf unter vier Wörtern

# Wir müssen auch noch weitere sachen filtern, etwa "weitere Alternativen gesucht" und
# "senden Sie Ihren Vorschlag über das Kontaktformular

missing_entries_2 <- "senden Sie Ihren Vorschlag über das Kontaktformular|weitere Alternativen gesucht|senden Sie Ihren Vorschlag^1"

gender_dict_clean <- gender_dict %>%
  mutate(word_count = str_count(word_list, "\\w+")) %>%
  filter(word_count <= 4) %>%
  filter(!str_detect(alternative_list, missing_entries_2)) %>%
  mutate(word_list = str_remove_all(word_list, "\\.\\.\\. "),
         alternative_list = str_remove_all(alternative_list, "\\.\\.\\. ")) %>%
  mutate(word_list = str_remove_all(word_list, "\\.\\.\\.$"),
         alternative_list = str_remove_all(alternative_list, "\\.\\.\\.$"))

# Weitere Veränderungen müssen manuell vorgenommen werden.

write_csv(gender_dict_clean %>% select(word_list, alternative_list), "data/gender_dict.csv")
