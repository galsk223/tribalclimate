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
library(conflicted)

conflict_prefer("select","dplyr")
conflict_prefer("filter","dplyr")

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
  dplyr::select(GEOID10,AllArea_GasPortion)
oil <- read_rds("01_data/clean/f_oil_wells_block.rds") %>% 
  dplyr::select(GEOID10,Area,AllArea_OilPortion)
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

tribe_bridge <- read_csv("01_data/cache/tribe_bg_full_integrated_combined_FINAL.csv",
                         col_types = "ccccccccc") %>%
  select(tribe_id=tribe,
         tribe_census_id=census_tribe,
         GEOID10=geoid10,
         fips,
         county,
         state)

# Rename fields, drop units, replace NAs when appropriate
final_ds <- all %>%
  select(tribe,
         GEOID10,
         heatdays=Mean_HeatDays,
         drought=`1980-2020_PDSI_Mean`,
         precip,
         whp=WHP,
         oil_portion=AllArea_OilPortion,
         gas_portion=AllArea_GasPortion,
         og_basin_portion=BasinPortion,
         federal_lands_portion=PADPortion,
         soc=Interpolated_15,
         elevation=elevation_mean,
         tri=tri_mean) %>%
  units::drop_units() %>%
  mutate(across(contains("portion"),~ifelse(is.na(.),0,.))) %>%
  inner_join(tribe_bridge,.,by="GEOID10")

write_csv(final_ds,"01_data/clean/tribal_dispossession_block.csv")
write_csv(final_ds,"/RSTOR/tribal_climate/data_products/tribal_dispossession_block.csv")


# Append geography and export as geopackage
final_ds_geo <- inner_join(select(tribedf,GEOID10),final_ds,by="GEOID10")
  
write_sf(final_ds_geo,"01_data/clean/tribal_dispossession_block.gpkg")
file.copy("01_data/clean/tribal_dispossession_block.gpkg",
          "/RSTOR/tribal_climate/data_products/tribal_dispossession_block.gpkg",recursive = T)

#Calculate area from spatial file
# tribedf %>%
#   slice(1:100) %>%
#   mutate(area=st_area(geometry)) %>%
#   View()

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

