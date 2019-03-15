library(tidyverse)
library(rvest)
library(furrr)
library(stringi)
plan(multiprocess)

page_all <- read_html("https://www.bundestag.de/ajax/filterlist/de/abgeordnete/-/525246/h_e3c112579919ef960d06dbb9d0d44b67?limit=9999&view=BTBiographyList")

get_mdb_links <- function(x){
  tibble(
    Name = x %>% html_nodes("h3") %>% html_text(trim = TRUE),
    Fraktion = x %>% html_nodes(".bt-person-fraktion") %>% html_text(trim = TRUE),
    Link = x %>% html_nodes("a") %>% html_attr("href") %>% paste0("https://www.bundestag.de", .)
  )
}

mdb_data <- get_mdb_links(page_all)

mdb_data <- mdb_data %>%
  mutate(status = case_when(
    str_detect(Fraktion, "\\*\\*") ~ "verstorben",
    str_detect(Fraktion, "\\*")    ~ "ausgeschieden",
    TRUE                           ~ "aktiv"))

# Wir brauchen die Wahlkreise noch

get_mdb_wahlkreis <- function(x){
  page <- read_html(x)
  
  tibble(
    Link = x,
    wahlkreis = page %>% html_nodes("#bt-landesliste-collapse .bt-link-intern") %>% html_text(trim = TRUE))
}

mdb_wahlkreis <- future_map_dfr(mdb_data$Link, get_mdb_wahlkreis, .progress = TRUE)

# Alles in Klammern der Namen löschen

mdb_df_full <- mdb_data %>%
  mutate(Name = str_remove(Name, "\\(.*?\\)")) %>%
  left_join(mdb_wahlkreis) %>%
  mutate(Adresse_BT = paste0(Name, "\n", "Deutscher Bundestag \nPlatz der Republik 1 \n11011 Berlin"))

# E-Mail-Adressen generieren
mdb_df_full <- mdb_df_full %>%
  mutate(name_simple = str_remove_all(Name, "Dr. ")) %>%
  mutate(name_simple = str_remove_all(name_simple, "Prof. ")) %>%
  mutate(name_simple = str_remove_all(name_simple, "h. c. ")) %>%
  mutate(vorname_email = str_extract(name_simple, ",\\s(.*?)(\\s|$)")) %>% # Alles nach Komma und Leerzeichen oder Ende des Strings
  mutate(vorname_email = str_remove(vorname_email, ",\\s")) %>%
  mutate(vorname_email = str_remove(vorname_email, "\\s$")) %>%
  mutate(nachname_email = str_extract(Name, "(.*?),\\s")) %>%
  mutate(nachname_email = str_remove(nachname_email, ",\\s")) %>%
  mutate(nachname_email = str_remove(nachname_email, "\\s$")) %>%
  mutate(email_extra = case_when(str_detect(Name, "\\svon\\sder") ~ "vonder",
                                 str_detect(Name, "\\svon") ~ "von",
                                 str_detect(Name, "\\de") ~ "de",
                                 TRUE ~ NA_character_)) %>%
  mutate(email = ifelse(is.na(email_extra),
                        paste0(vorname_email, ".", nachname_email, "@bundestag.de"),
                        paste0(vorname_email, ".", email_extra, nachname_email, "@bundestag.de"))) %>%
  mutate(email = tolower(email)) %>%
  mutate(email = stri_replace_all_fixed(email, c("ä", "ö", "ü", "é", "è", "ß", "ğ"), 
    c("ae", "oe", "ue", "e", "e", "ss", "g"), vectorize_all = FALSE)) %>% # meh umlaute
  filter(status == "aktiv") %>%
  select(-vorname_email, -nachname_email, -email_extra, -status)


setwd("/home/josef/Documents/")

write_csv(mdb_df_full, "Daten_MdB_alle.csv")
