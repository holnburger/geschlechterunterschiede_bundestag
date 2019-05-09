library(rvest)
library(tidyverse)

# Fraktionsvorsitzende CDU/CSU

page_cducsu <- read_html("https://www.bundestag.de/parlament/fraktionen/cducsu/cducsu/245192")

vorsitz_cducsu <- page_cducsu %>% 
  html_nodes(xpath = '//*[contains(concat( " ", @class, " " ), concat( " ", "bt-standard-content", " " ))]//*[contains(concat( " ", @class, " " ), concat( " ", "bt-standard-content", " " ))]//p[(((count(preceding-sibling::*) + 1) = 2) and parent::*)]//a | //p[(((count(preceding-sibling::*) + 1) = 4) and parent::*)]//a | //p[(((count(preceding-sibling::*) + 1) = 6) and parent::*)]//a') %>%
  html_text() %>%
  data_frame(name = .,fraktion = "CDU/CSU")

# Fraktionsvorsitzende SPD

page_spd <- read_html("https://www.bundestag.de/parlament/fraktionen/spd/spd/245190")

vorsitz_spd <- page_spd %>%
  html_nodes('p:nth-child(4) a , .bt-standard-content .bt-standard-content p:nth-child(2) a') %>%
  html_text() %>%
  data_frame(name = ., fraktion = "SPD")

# Fraktionsvorsitzende Grüne

page_gruene <- read_html("https://www.bundestag.de/parlament/fraktionen/gruene/gruene/245196")

vorsitz_gruene <- page_gruene %>%
  html_nodes('p:nth-child(9) a , .bt-standard-content .bt-standard-content p:nth-child(2) a') %>%
  html_text() %>%
  data_frame(name = ., fraktion = "BÜNDNIS 90/DIE GRÜNEN")

# Fraktionsvorsitzende Linke

page_linke <- read_html("https://www.bundestag.de/parlament/fraktionen/linke/linke/245198")

vorsitz_linke <- page_linke %>%
  html_nodes('p:nth-child(8) a , a a , p:nth-child(6) a , .bt-standard-content .bt-standard-content p:nth-child(2) a') %>%
  html_text() %>%
  data_frame(name = ., fraktion = "DIE LINKE")

# Fraktionsvorsitzende FDP

page_fdp <- read_html("https://www.bundestag.de/parlament/fraktionen/fdp-inhalt/527364")

vorsitz_fdp <- page_fdp %>%
  html_nodes('p:nth-child(8) a , .bt-standard-content .bt-standard-content p:nth-child(2) a') %>%
  html_text() %>%
  data_frame(name = ., fraktion = "FDP")

# Fraktionsvorsitzende AfD

page_afd <- read_html("https://www.bundestag.de/parlament/fraktionen/afd-inhalt/527374")

vorsitz_afd <- page_afd %>%
  html_nodes('p:nth-child(4) a , .bt-standard-content .bt-standard-content p:nth-child(2) a') %>%
  html_text() %>%
  data_frame(name = ., fraktion = "AfD")


# Dataframe aller Fraktionsvorsitzender und Stellvertreter

fraktionsvorsitz <- bind_rows(vorsitz_cducsu,
                              vorsitz_spd,
                              vorsitz_gruene,
                              vorsitz_linke,
                              vorsitz_fdp,
                              vorsitz_afd) %>%
  unite(name, fraktion, sep = ", ", col = "name") %>%
  mutate(fraktionsvorsitz = 1)
  

write_csv(fraktionsvorsitz, "data/BT_19/fraktionsvorsitzende_u_stellv.csv")
