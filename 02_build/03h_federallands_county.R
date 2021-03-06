# for federal lands, filtered to manager type == federal
# see doc in folder for more info


setwd("/home/sambk223/tribalclimate")

library(raster)
library(tidyverse)
library(data.table)
library(conflicted)
library(lubridate)
library(sf)
library(tigris)
library(RPostgreSQL)
library(dbplyr)
library(RcppRoll)
library(scales)
library(broom)
library(dotwhisker)
library(janitor)
library(directlabels)
library(googledrive)
library(cowplot)
library(plotly)
library(rmarkdown)
library(readxl)
library(SafeGraphR)
library(vroom)
library(ncdf4)
library(furrr)
library(elevatr)

conflict_prefer("filter", "dplyr")
conflict_prefer("between", "dplyr")
conflict_prefer("last", "dplyr")
conflict_prefer("lag", "dplyr")
conflict_prefer("select", "dplyr")
conflict_prefer("year", "lubridate")
conflict_prefer("ggsave", "ggplot2")

Sys.setenv(RSTUDIO_PANDOC="/usr/lib/rstudio-server/bin/pandoc")


rm(list = ls())

tribedf <- read_rds("01_data/cache/tribe_shapefiles_micro.rds") %>% 
  filter(!state %in% c("02","15")) %>% 
  st_transform(5070) %>% 
  mutate(Area = st_area(geometry)) %>% 
  st_transform(4269)

tribeusebuffed <- tribedf %>% 
  st_transform(5070) %>% 
  st_buffer(160.934) %>% 
  mutate(BuffedArea = st_area(geometry))

regions <- 1:10
r <- 1

pad <- map_dfr(regions,function(r){
  
  temp <- read_sf(paste0("01_data/PAD_raw/PADUS2_1_Region",r,"_Shapefile/",
                         "PADUS2_1Combined_Tribal_DOD_Fee_Designation_Easement_Region",r,".shp")) %>% 
    filter(Mang_Type == "FED")
  
  PADtribe <- tribeusebuffed %>% 
    st_intersection(st_buffer(temp,0),.) %>% 
    mutate(PADArea = st_area(geometry),
           PADPortion = PADArea/BuffedArea) %>% 
    st_set_geometry(NULL) %>% 
    dplyr::select(GEOID,BuffedArea,PADArea,PADPortion)
  
  print(r)
  return(PADtribe)
  
})

padsave <- pad %>% 
  group_by(GEOID) %>% 
  summarise(PADPortion = as.numeric(sum(PADArea)/sum(BuffedArea))) %>% 
  ungroup() %>% 
  left_join(tribeuse,.,by="GEOID") %>% 
  st_set_geometry(NULL) %>% 
  replace_na(list(PADPortion = 0))

write_rds(padsave,"01_data/clean/h_federalland_county.rds")  
