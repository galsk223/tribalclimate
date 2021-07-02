
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

basins <- read_sf("01_data/SedimentaryBasins_US_EIA/SedimentaryBasins_US_May2011_v2.shp") %>% 
  st_transform(4269)

county <- tribedf %>% 
  group_split(GEOID_county)
c <- county[[200]]

plan(multisession, workers = 4)
future_map(county,function(c){
  
  tribe_basins <- st_intersection(basins,c) 
  
  if(nrow(tribe_basins) > 0){
    
    cache <- tribe_basins %>% 
      mutate(BasinArea = st_area(geometry),
             BasinPortion = as.numeric(BasinArea/Area)) %>% 
      st_set_geometry(NULL) %>% 
      replace_na(list(BasinPortion = 0)) %>% 
      mutate(BasinPortion = ifelse(BasinPortion > 1, 1, BasinPortion),
             BasinBinary = ifelse(BasinPortion > 0, 1, 0))
   
    write_rds(cache,paste0("01_data/cache/g_OilGas_basins_block/",unique(c$GEOID_county),".rds"))
     
  }
  
})
plan(sequential)
gc()

fl <- list.files("01_data/cache/g_OilGas_basins_block", full.names = T)
save <- map_dfr(fl,function(f){
  
  temp <- read_rds(f)
  
})
write_rds(save,"01_data/clean/g_oilgas_basins_block.rds")
