
# Setup -------------------------------------------------------------------

library(xml2)
library(rvest)
library(tidyverse)
library(furrr)
plan(multiprocess)

# Overview of the speeches (13th to 18th Bundestag) -----------------------

prot <- read_xml("raw/prot_13-18/16/BT_16_004.xml") %>%
  xml_find_all("//sp") 

overview_df <- function(x){
  name <- x %>% xml_attr("name")
  group <- x %>% xml_attr("parliamentary_group")
  party <- x %>% xml_attr("party")
  role <- x %>% xml_attr("role")
  position <- x %>% xml_attr("position")
  period <- x %>% xml_find_first("//legislativePeriod") %>% xml_integer()
  session <- x %>% xml_find_first("//sessionNo") %>% xml_integer()
  date_session <- x %>% xml_find_first("//date") %>% xml_text() %>% as.Date()
  text <- map(x, ~xml_nodes(., "p") %>% xml_text() %>% str_c(collapse = "\n")) %>% as.character()
  interjections <- map(x, ~xml_nodes(., "stage") %>% xml_text())
  
  data_frame(name, group, party, role, position, period, session, date_session, text, interjections) %>%
    mutate_if(is.character, na_if, "NA") %>%
    mutate(speech_length = nchar(text)) %>%
    mutate(speaker = ifelse(role == "mp", 
                            paste0(name, " [", group, "]"),
                            paste0(name, " (", position, ")")))
}

# speeches of the 13th to 18th Bundestag ----------------------------------

file_paths <- paste0("raw/prot_13-18/", 13:18)

prot_files <- map(file_paths, ~list.files(., full.names = TRUE)) %>%
  unlist()

speech_extract <- map(prot_files, ~read_html(.) %>% xml_find_all("//sp"))

speech_df <- future_map_dfr(speech_extract, ~overview_df(.), .progress = TRUE)

write_rds(speech_df, "data/BT_13-18/speeches.RDS")
