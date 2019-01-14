# Setup -------------------------------------------------------------------

library(xml2)
library(rvest)
library(tidyverse)


# clean MdB data ----------------------------------------------------------

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

write_rds(mdb_data, "data/mdb_data.RDS")

# functions for protocol cleaning -----------------------------------------

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
  
  data_frame(raw, rede, id, vorname, nachname, fraktion, rolle, typ, status) %>%
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
  
  data_frame(rede_id, redner_id, redner_vorname, redner_nachname, redner_fraktion, redner_rolle, sitzung, datum, wahlperiode)
}

# prepare the protocol data (19th Bundestag) ----------------------------------------

prot_files <- list.files("raw/prot_19/", full.names = TRUE)

prot_extract <- map(prot_files, ~read_html(.) %>% xml_find_all("//rede"))

class(prot_extract) <- "xml_nodeset"

# overview of the protocols (19th Bundestag) ------------------------------

prot_overview <- map_dfr(prot_extract, ~get_overview_df(.))

# clean data because of misspellings

prot_overview <- prot_overview %>%
  mutate(redner_fraktion = ifelse(redner_fraktion == "Bündnis 90/Die Grünen", "BÜNDNIS 90/DIE GRÜNEN", redner_fraktion)) %>%
  mutate(redner_fraktion = ifelse(redner_fraktion == "Bremen", NA, redner_fraktion)) 

write_rds(prot_overview, "data/BT_19/overview.RDS")

# speeches in the 19th Bundestag ------------------------------------------

speech_extract <- map(prot_files, ~read_html(.) %>% xml_find_all("//rede/*"))

class(speech_extract) <- "xml_nodeset"

prot_speeches <- map_dfr(speech_extract, ~get_speeches_df(.)) %>%
  mutate(fraktion = ifelse(fraktion == "Bündnis 90/Die Grünen", "BÜNDNIS 90/DIE GRÜNEN", fraktion)) %>%
  mutate(fraktion = ifelse(fraktion == "Bremen", NA, fraktion)) 

write_rds(prot_speeches, "data/BT_19/speeches.RDS")

