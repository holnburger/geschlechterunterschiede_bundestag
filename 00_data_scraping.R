# Setup -------------------------------------------------------------------

library(xml2)
library(rvest)
library(tidyverse)

# Daten der Bundestagsabgeordneten-----------------------------------------

link_zip <- "https://www.bundestag.de/blob/472878/e207ab4b38c93187c6580fc186a95f38/mdb-stammdaten-data.zip"

download.file(link_zip, file.path("raw/mdb", basename(link_zip)))

unzip("raw/mdb/mdb-stammdaten-data.zip", exdir = "raw/mdb/")

# Download der Protokolle des 19. BT --------------------------------------

bt_website <- "https://www.bundestag.de/ajax/filterlist/de/services/opendata/543410-543410"

last_protocol <- bt_website %>% 
  read_html() %>%
  xml_find_first("//strong") %>% 
  xml_text(trim = TRUE) %>%
  str_extract("\\d+")

prot_websites <- paste0(bt_website, "?offset=", seq(0, last_protocol, 5))

get_prot_links <- function(x){
  x %>%
    read_html() %>%
    html_nodes(".bt-link-dokument") %>%
    html_attr("href") %>%
    paste0("https://www.bundestag.de", .)
}

get_prot_links(bt_website)

prot_links <- map(prot_websites, ~get_prot_links(.)) %>% unlist()

prot_links %>% map(~download.file(., file.path("raw/prot_19", basename(.))))


# Protokolle des 13 bis 18. Bundestags ---------------------------------

# Kopie in raw/prot_13-18 
# Source: https://github.com/PolMine/GermaParlTEI by Andreas Blätte.
# Wird allerdings nicht für die weitere Arbeit genutzt, da nur der 19. BT ausgewertet wird.
