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


if(!dir.exists("01_data/cache/h_federallands")){
  dir.create("01_data/cache/h_federallands")
}



tribeuse <- tribedf %>% 
  group_split(GEOID_county)

t <- tribeuse[[1]]

regions <- 1:10
r <- 1

plan(multisession, workers = 4)
future_map(tribeuse,function(t){
  
  tribeusebuffed <- t %>% 
    st_transform(5070) %>% 
    st_buffer(160.934) %>% 
    mutate(BuffedArea = st_area(geometry))
  
  pad <- map_dfr(regions,function(r){
    
    temp <- read_sf(paste0("01_data/PAD_raw/PADUS2_1_Region",r,"_Shapefile/",
                           "PADUS2_1Combined_Tribal_DOD_Fee_Designation_Easement_Region",r,".shp")) %>% 
      filter(Mang_Type == "FED")
    
    PADtribe <- tribeusebuffed %>% 
      st_intersection(st_buffer(temp,0),.) %>% 
      mutate(PADArea = st_area(geometry),
             PADPortion = PADArea/BuffedArea) %>% 
      st_set_geometry(NULL) %>% 
      dplyr::select(GEOID10,BuffedArea,PADArea,PADPortion)
    
    print(r)
    return(PADtribe)
    
  })
  
  write_rds(pad,paste0("01_data/cache/h_federallands/",unique(t$GEOID_county),".rds"))
  
})
plan(sequential)
gc()

fl <- list.files("01_data/cache/h_federallands", full.names = T)
save <- map_dfr(fl, function(f){
  
  r <- read_rds(f)
  
})

write_rds(save,"01_data/clean/h_federallands_blocks.rds")
