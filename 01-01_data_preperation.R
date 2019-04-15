# Setup -------------------------------------------------------------------

library(xml2)
library(rvest)
library(tidyverse)
library(knitr)
library(kableExtra)
require(scales)

# MdB Daten bereinigen -----------------------------------------------------

file_raw <- read_xml("raw/mdb/MDB_STAMMDATEN.XML")

get_data_mdb <- function(x){
  id <- x %>% xml_find_all("//ID") %>% xml_text()
  geschlecht <- x %>% xml_find_all("//ID/following-sibling::BIOGRAFISCHE_ANGABEN/GESCHLECHT") %>% xml_text()
  geburtsjahr <- x %>% xml_find_all("//ID/following-sibling::BIOGRAFISCHE_ANGABEN/GEBURTSDATUM") %>% xml_text() %>% as.integer()
  partei <- x %>% xml_find_all("//ID/following-sibling::BIOGRAFISCHE_ANGABEN/PARTEI_KURZ") %>% xml_text()
  wahlperioden <- x %>% xml_find_all("//ID/following-sibling::WAHLPERIODEN")
  namen <- x %>% xml_find_all("//ID/following-sibling::NAMEN")
  
  tibble(id, geschlecht, geburtsjahr, partei, wahlperioden) %>%
    mutate(fraktion = map(wahlperioden, 
                          ~xml_find_all(., ".//INSTITUTIONEN/INSTITUTION/INSART_LANG[contains(.,'Fraktion/Gruppe')]") %>%
                            xml_parent() %>% xml_nodes("INS_LANG") %>% xml_text())) %>%
    #mutate(fraktion_mitglied_von = map(wahlperioden, 
    #                      ~xml_find_all(., ".//INSTITUTIONEN/INSTITUTION/INSART_LANG[contains(.,'Fraktion/Gruppe')]") %>%
    #                        xml_parent() %>% xml_nodes("MDBINS_VON") %>% xml_text())) %>%
    #mutate(fraktion_mitglied_bis = map(wahlperioden, 
    #                                   ~xml_find_all(., ".//INSTITUTIONEN/INSTITUTION/INSART_LANG[contains(.,'Fraktion/Gruppe')]") %>%
    #                                     xml_parent() %>% xml_nodes("MDBINS_BIS") %>% xml_text())) %>%
    mutate(wahlperioden = map(wahlperioden, ~xml_nodes(., "WP") %>% xml_text() %>% as.integer())) %>%
    mutate(anzahl_wahlperioden = map(wahlperioden, ~length(.)) %>% unlist()) %>%
    mutate(nachname = map(namen, ~xml_nodes(., "NACHNAME") %>% xml_text())) %>%
    mutate(vorname = map(namen, ~xml_nodes(., "VORNAME") %>% xml_text()))
}

mdb_data <- get_data_mdb(file_raw)

# Korrekturdaten
# Carsten Träger, Marja-Lisa Völlers und Gisela Manderla haben teilweise falsche Angaben zur ID in den Protokollen

corr_traeger <- mdb_data %>%
  filter(id == "11004426") %>%
  mutate(id = "999190001")

corr_voellers <- mdb_data %>%
  filter(id == "11004942") %>%
  mutate(id = "10000")

corr_manderla <- mdb_data %>%
  filter(id == "11004348") %>%
  mutate(id = "999980200")

mdb_data <- mdb_data %>%
  bind_rows(corr_traeger) %>%
  bind_rows(corr_voellers) %>%
  bind_rows(corr_manderla)

write_rds(mdb_data, "data/mdb_data.RDS")

# Funktionen für die Bereinigung der Protokolle -----------------------------------

get_speeches_df <- function(x){
  raw <- x
  rede <- x %>% xml_text()
  id <- x %>% xml_node("redner") %>% xml_attr("id")
  vorname <- x %>% xml_node("vorname") %>% xml_text()
  nachname <- x %>% xml_node("nachname") %>% xml_text()
  fraktion <- x %>% xml_node("fraktion") %>% xml_text()
  rolle <- x %>% xml_node("rolle_kurz") %>% xml_text()
  typ <- x %>% xml_name()
  status <- x %>% xml_attr("klasse")
  
  tibble(raw, rede, id, vorname, nachname, fraktion, rolle, typ, status) %>%
    mutate(rede_id = map(raw, ~xml_parent(.) %>% xml_attr("id")) %>% as.character()) %>%
    select(-raw) %>%
    mutate(status = ifelse(typ == "kommentar", typ, status)) %>%
    mutate(status = ifelse(typ == "name", "präsidium", status)) %>%
    mutate(fraktion = case_when(
      typ == "name"       ~ "präsidium",
      !is.na(rolle)       ~ "andere",
      TRUE                ~ fraktion)) %>%
    fill(id, vorname, nachname, fraktion) %>%
    mutate(präsidium = ifelse(fraktion == "präsidium", TRUE, FALSE)) %>%
    mutate(fraktion = ifelse(fraktion == "präsidium", NA, fraktion)) %>%
    filter(!status %in% c("T_NaS", "T_Beratung", "T_fett", "redner")) %>%
    filter(!typ %in% c("a", "fussnote", "sup")) %>%
    select(rede_id, rede, id, vorname, nachname, fraktion, präsidium, typ, status)
}

get_overview_df <- function(x){
  rede_id <- x %>% xml_attr("id")
  redner_id <- x %>% xml_node("redner") %>% xml_attr("id")
  redner_vorname <- x %>% xml_node("redner") %>% xml_node("vorname") %>% xml_text()
  redner_nachname <- x %>% xml_node("redner") %>% xml_node("nachname") %>% xml_text()
  redner_fraktion <- x %>% xml_node("redner") %>% xml_node("fraktion") %>% xml_text()
  redner_rolle <- x %>% xml_node("rolle_kurz") %>% xml_text()
  sitzung <- x %>% xml_find_first("//sitzungsnr") %>% xml_text() %>% as.integer()
  datum <- x %>% xml_find_first("//datum") %>% xml_attr("date") %>% lubridate::dmy()
  wahlperiode <- x %>% xml_find_first("//wahlperiode") %>% xml_text() %>% as.integer()
  
  tibble(rede_id, redner_id, redner_vorname, redner_nachname, redner_fraktion, redner_rolle, sitzung, datum, wahlperiode)
}

# Vorbereitung der Protokolldaten des 19. Bundestages -------------------------------

prot_files <- list.files("raw/prot_19/", full.names = TRUE)

prot_extract <- map(prot_files, ~read_html(.) %>% xml_find_all("//rede"))

class(prot_extract) <- "xml_nodeset"

# Überblick der Protokolldaten ------------------------------

prot_overview <- map_dfr(prot_extract, get_overview_df)

# Fehler in den Protokollen ausbessern (Falsche Dokumentation/Rechtschreibfehler)

prot_overview <- prot_overview %>%
  mutate(redner_fraktion = ifelse(redner_fraktion == "Bündnis 90/Die Grünen", "BÜNDNIS 90/DIE GRÜNEN", redner_fraktion)) %>%
  mutate(redner_fraktion = ifelse(redner_fraktion == "Bremen", NA, redner_fraktion)) 

write_rds(prot_overview, "data/BT_19/overview.RDS")

# Reden des 19. Bundestages ------------------------------------------

speech_extract <- map(prot_files, ~read_html(.) %>% xml_find_all("//rede/*"))

class(speech_extract) <- "xml_nodeset"

# Abermals Rechtschreibfehler/falsche Dokumentation ausbessern

prot_speeches <- map_dfr(speech_extract, get_speeches_df) %>%
  mutate(fraktion = ifelse(fraktion == "Bündnis 90/Die Grünen", "BÜNDNIS 90/DIE GRÜNEN", fraktion)) %>%
  mutate(fraktion = ifelse(fraktion == "Bremen", NA, fraktion)) 

write_rds(prot_speeches, "data/BT_19/speeches.RDS")

# Überblick der Reden nach Geschlecht für das Dokument
# Insgesamt 8.764 Reden, untersucht werden aber nur reden von Parlamentariern, nicht die der Regierung oder von Gästen
# Hintergrund: Zum einen liegen nur von den MdBs die Daten vor - und es handelt sich hier auch um inhaltliche Debatten
# zu Themen des BT.

# Anzahl der Reden von Parlamentariern
prot_overview %>% filter(is.na(redner_rolle)) %>% nrow()
# 8.294 Reden

# Geschlecht der Redner*innen
prot_overview %>%
  filter(is.na(redner_rolle)) %>%
  left_join(mdb_data, by = c("redner_id" = "id")) %>%
  count(geschlecht) %>%
  rename(Geschlecht = geschlecht, Reden = n) %>%
  mutate(Anteil = scales::percent(Reden/sum(Reden), decimal.mark = ",")) %>%
  mutate(Reden = scales::comma(Reden, big.mark = ".", decimal.mark = ",")) %>%
  knitr::kable(format = "latex", booktabs = TRUE, linesep = "") %>%
  write_file(., "document/tables/uebersicht_reden.tex")

# Übersicht der Sitzanteile von Frauen und Anteil der Reden nach Fraktion
# Quelle Kürschner (2019), abgewandelt
freq_seats <- tibble::tribble( ~fraktion, ~Frauen, ~Männer, ~gesamt,
                              "CDU/CSU",      51,     195,     246,
                                  "SPD",      65,      87,     152,
                                  "AfD",      10,      81,      91,
                                  "FDP",      19,      61,      80,
                            "Die Linke",      37,      32,      69,
               "Bündnis 90/ Die Grünen",      39,      28,      67,
                         "fraktionslos",       1,       3,       4
               ) %>%
  mutate(freq_seats = scales::percent(Frauen/gesamt, 
                                           big.mark = ".", decimal.mark = ","))

# Anteil Reden von Frauen nach Fraktion, Namen müssen zum Ende angepasst werden
freq_speeches <- prot_overview %>%
  filter(!is.na(redner_fraktion)) %>%
  left_join(mdb_data, by = c("redner_id" = "id")) %>%
  group_by(redner_fraktion, geschlecht) %>%
  summarise(n = n()) %>%
  mutate(freq_speeches = scales::percent(n / sum(n),
                         big.mark = ".", 
                         decimal.mark = ",", 
                         accuracy = 0.1)) %>%
  filter(geschlecht == "weiblich") %>%
  rename(fraktion = redner_fraktion) %>%
  ungroup(fraktion) %>%
  mutate(fraktion = ifelse(fraktion == "BÜNDNIS 90/DIE GRÜNEN",
                           "Bündnis 90/ Die Grünen", 
                           fraktion)) %>%
  mutate(fraktion = ifelse(fraktion == "DIE LINKE",
                           "Die Linke", 
                           fraktion))

# Tabelle zur Übersicht
freq_seats %>%
  left_join(freq_speeches) %>%
  select(fraktion, freq_seats, freq_speeches) %>%
  rename(Fraktion = fraktion,
         "Sitzanteil\\Frauen" = freq_seats,
         "Redenanteil\\nFrauen" = freq_speeches) %>%
  knitr::kable(format = "latex", booktabs = TRUE, linesep = "") %>%
  write_file(., "document/tables/vergleich_sitze_reden.tex")
