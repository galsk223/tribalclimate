

#Prototyping tribal areas
library(pacman)
p_load(tidyverse,lubridate,janitor,tigris)

setwd("L:/My Drive/Projects/tribal")

#############################################
#Testing on Burns OR
tbg_list <- read_delim("burns_paiute.txt",delim = ";",col_types = "cccccccc") %>%
  clean_names()

paiute <- tigris::blocks(state = "41",
                         county = "025",
                         class="sf") %>%
  clean_names()


tbg <- tbg_list %>%
  mutate(geoid10=str_c(state,
                     county,
                     str_remove(tract,"\\."),
                     block)) %>%
  inner_join(paiute,.)

mapview::mapview(tbg) %>%
  mapview::mapshot(url = "paiute.html")


