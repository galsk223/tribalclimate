library(tidyverse)
library(data.table)
library(janitor)
library(sf)
library(mapview)
library(raster)
library(terra)
library(lubridate)
library(downloader)
library(ncdf4)
library(elevatr)
library(furrr)

rm(list = ls())


tribedf <- read_rds("01_data/cache/tribe_shapefiles_micro.rds") %>% 
  filter(!state %in% c("02","15"))


heat <- read_rds("01_data/clean/a_heat_blocks.rds")
drought <- read_rds("01_data/clean/b_drought_blocks.rds") %>% 
  dplyr::select(-tribe)
precip <- read_rds("01_data/clean/c_precip_blocks.rds")
whp <- read_rds("01_data/clean/d_whp_blocks.rds")
elrug <- read_rds("01_data/clean/e_elevrugg_blocks.rds")
gas <- read_rds("01_data/clean/f_gas_wells_block.rds") %>% 
  dplyr::select(GEOID10,AllArea_Gas)
oil <- read_rds("01_data/clean/f_oil_wells_block.rds") %>% 
  dplyr::select(GEOID10,AllArea_Oil)
basins <- read_rds("01_data/clean/g_oilgas_basins_block.rds") %>% 
  dplyr::select(GEOID10,BasinPortion,BasinBinary)
pad <- read_rds("01_data/clean/h_federallands_blocks.rds") %>% 
  dplyr::select(GEOID10,PADPortion)
soc <- read_rds("01_data/clean/i_soc_blocks.rds")

all <- left_join(heat,drought,by="GEOID10") %>% 
  left_join(.,precip,by="GEOID10") %>% 
  left_join(.,whp,by="GEOID10") %>% 
  left_join(.,elrug,by="GEOID10") %>% 
  left_join(.,gas,by="GEOID10") %>% 
  left_join(.,oil,by="GEOID10") %>% 
  left_join(.,basins,by="GEOID10") %>% 
  left_join(.,pad,by="GEOID10") %>% 
  left_join(.,soc,by="GEOID10") 


# sums <- all %>% 
#   dplyr::select(-contains(c("q25","q75","sd","min","median","max","UID"))) %>% 
#   summarise_all(list(
#     N = ~sum(!is.na(.)),
#     Min = ~min(., na.rm = T),
#     Mean = ~mean(., na.rm = T),
#     Max = ~max(., na.rm = T))) %>% 
#   pivot_longer(everything()) %>% 
#   mutate(Stat = str_remove(str_extract(name,"_N|_Min|_Mean|_Max"),"_"),
#          Variable = str_remove(str_remove(name,"_N|_Min|_Mean|_Max"),"_"),
#          value = round(value,3)) %>% 
#   pivot_wider(-name,
#               names_from = Stat)

write_csv(all,"01_data/clean/00_all_newpolygons_blocks.csv")
