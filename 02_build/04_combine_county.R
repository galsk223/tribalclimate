
rm(list = ls())
tribedf <- read_rds("01_data/cache/tribe_county_shapefiles.rds")
tribecounties <- tribedf %>% 
  dplyr::select(GEOID) %>% 
  unique()

empty <- tribecounties %>% 
  mutate(area = as.numeric(st_area(geometry))) %>% 
  st_set_geometry(NULL) %>% 
  filter(area == 0)

tribeuse <- tribecounties %>% 
  filter(!GEOID %in% empty$GEOID)



heat <- read_rds("01_data/clean/a_heat_gridmet_county.rds")
drought <- read_rds("01_data/clean/b_drought_county.rds") %>% 
  rename(drought_mean = `1980-2020 Mean`)
precip <- read_rds("01_data/clean/c_precip_county.rds")
whp <- read_rds("01_data/clean/d_whp_county.rds")
elrug <- read_rds("01_data/clean/e_ElevationAndRuggedness_County.rds")

wells_oil <- read_rds("01_data/clean/f_Oil_wells_county.rds") %>% 
  dplyr::select(GEOID,AllArea_OilPortion) %>% 
  mutate(AllArea_OilPortion = as.numeric(AllArea_OilPortion)) %>% 
  unique() %>% 
  left_join(tribeuse,.,by="GEOID") %>% 
  st_set_geometry(NULL) %>% 
  replace(is.na(.), 0)

wells_gas <- read_rds("01_data/clean/f_Gas_wells_county.rds") %>% 
  dplyr::select(GEOID,AllArea_GasPortion) %>% 
  mutate(AllArea_GasPortion = as.numeric(AllArea_GasPortion)) %>% 
  unique() %>% 
  left_join(tribeuse,.,by="GEOID") %>% 
  st_set_geometry(NULL) %>% 
  replace(is.na(.), 0)
  
OGbasins <- read_rds("01_data/clean/g_OilGas_basins_county.rds") %>% 
  st_set_geometry(NULL) 
PAD <- read_rds("01_data/clean/h_federalland_county.rds")

soc <- map_dfr(list.files("01_data/cache/soc_county2", full.names = T),
               function(fl){
                 
                 t <- read_rds(fl)
                 
               }) %>% 
  group_by(GEOID) %>% 
  summarise(SOC_mean = mean(Interpolated_15)) %>% 
  ungroup()

all <- tribedf %>% 
  st_set_geometry(NULL) %>% 
  left_join(.,heat,by="GEOID") %>% 
  left_join(.,drought,by="GEOID") %>% 
  left_join(.,precip,by="GEOID") %>% 
  left_join(.,whp,by="GEOID") %>% 
  left_join(.,elrug,by="GEOID") %>% 
  left_join(.,wells_oil,by="GEOID") %>% 
  left_join(.,wells_gas,by="GEOID") %>% 
  left_join(.,OGbasins,by="GEOID") %>% 
  left_join(.,PAD,by="GEOID") %>% 
  left_join(.,soc,by="GEOID") 


# Rename fields, drop units, replace NAs when appropriate
final_ds <- all %>%
  select(tribe,
         GEOID,
         heatdays=heatdays_mean,
         drought=drought_mean,
         precip,
         whp=whp_mean,
         oil_portion=AllArea_OilPortion,
         gas_portion=AllArea_GasPortion,
         og_basin_portion=BasinPortion,
         federal_lands_portion=PADPortion,
         soc=SOC_mean,
         elevation=elevation_mean,
         tri=tri_mean) %>%
  inner_join(tigris::fips_codes %>% 
               mutate(GEOID=str_c(state_code,county_code),
                      county=str_remove(county,"County")) %>% 
               select(GEOID,state,county),.,
             by="GEOID")

write_csv(final_ds,"01_data/clean/tribal_dispossession_county.csv")
write_csv(final_ds,"/RSTOR/tribal_climate/data_products/tribal_dispossession_county.csv")


us_co <- USAboundaries::us_counties(resolution = "low")

# Append geography and export as geopackage
final_ds_geo <- inner_join(select(us_co,GEOID=geoid),final_ds,by="GEOID")

write_sf(final_ds_geo,"01_data/clean/tribal_dispossession_county.gpkg")




# sums <- all %>% 
#   dplyr::select(-contains(c("q25","q75","sd","min","median","max","GEOID","tribe"))) %>% 
#   summarise_all(list(
#                    N = ~sum(!is.na(.)),
#                    Min = ~min(., na.rm = T),
#                    Mean = ~mean(., na.rm = T),
#                    Max = ~max(., na.rm = T))) %>% 
#   pivot_longer(everything()) %>% 
#   mutate(Stat = str_remove(str_extract(name,"_N|_Min|_Mean|_Max"),"_"),
#          Variable = str_remove(str_remove(name,"_N|_Min|_Mean|_Max"),"_"),
#          value = round(value,3)) %>% 
#   pivot_wider(-name,
#               names_from = Stat)








                