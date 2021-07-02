
# CODING
## 1 - cell contains >= 1 oil, no gas 
## 2 - cell contains >= 1 gas, no oil
## 3 - cell contains >= 1 oil AND >= 1 gas 
## 4 - mystery wells

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

dec <- c("pre1900",paste0(seq(1900,2000,by=10),"s"))
d <- dec[1]


# -----------------------------------
# OIL
# -----------------------------------

wells_oil <- map_dfr(dec,function(d){
  
  raw <- read_sf(paste0("01_data/OilGas_RawData/uscells06g_timeslices/shape/",d,"_1sqmicg.shp")) %>% 
    st_transform(4269) %>% 
    filter(CELLSYMB == 1 |
             CELLSYMB == 3) %>% 
    mutate(Dec = d)

})

county <- tribedf %>% 
  group_split(GEOID_county)
# c <- county[[20]]

intribes_oil_unioned <- map_dfr(county,function(c){
  
  intribes_oil_df <- st_intersection(st_buffer(wells_oil,0),c)
  
  actGEOID <- unique(intribes_oil_df$GEOID10)
  aGEOID <- "300850801001004"
  
  intribes_oil_unioned_county <- map_dfr(actGEOID, function(aGEOID){
    
    temp <- intribes_oil_df %>% 
      filter(GEOID10 == aGEOID) %>% 
      dplyr::select(geometry) %>% 
      unique() %>%
      summarize(geometry = st_union(geometry)) %>% 
      mutate(AllArea_Oil = st_area(geometry),
             GEOID10 = aGEOID) %>% 
      st_set_geometry(NULL) %>% 
      left_join(., c, by=c("GEOID10")) %>% 
      mutate(AllArea_OilPortion = AllArea_Oil/Area) 
    
    print(match(aGEOID,actGEOID))
    return(temp)
    
  })
  
  return(intribes_oil_unioned_county)
  
})




# -----------------------------------
# GAS
# -----------------------------------

wells_gas <- map_dfr(dec,function(d){
  
  raw <- read_sf(paste0("01_data/OilGas_RawData/uscells06g_timeslices/shape/",d,"_1sqmicg.shp")) %>% 
    st_transform(4269) %>% 
    filter(CELLSYMB == 2 |
             CELLSYMB == 3) %>% 
    mutate(Dec = d)
  
})

intribes_gas_unioned <- map_dfr(county,function(c){
  
  intribes_gas_df <- st_intersection(st_buffer(wells_gas,0),c)
  
  actGEOID <- unique(intribes_gas_df$GEOID10)
  aGEOID <- "300850801001004"
  
  intribes_gas_unioned_county <- map_dfr(actGEOID, function(aGEOID){
    
    temp <- intribes_gas_df %>% 
      filter(GEOID10 == aGEOID) %>% 
      dplyr::select(geometry) %>% 
      unique() %>%
      summarize(geometry = st_union(geometry)) %>% 
      mutate(AllArea_Gas = st_area(geometry),
             GEOID10 = aGEOID) %>% 
      st_set_geometry(NULL) %>% 
      left_join(., c, by=c("GEOID10")) %>% 
      mutate(AllArea_GasPortion = AllArea_Gas/Area) 
    
    print(match(aGEOID,actGEOID))
    return(temp)
    
  })
  
  return(intribes_gas_unioned_county)
  
})



# -----------------------------------
# save
# -----------------------------------

write_rds(intribes_oil_unioned,"01_data/clean/f_oil_wells_block.rds")
write_rds(intribes_gas_unioned,"01_data/clean/f_gas_wells_block.rds")




