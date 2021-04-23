
rm(list = ls())

load("/RSTOR/tribal_climate/inputs/final_data.Rdata")

tribecounties <- merged_data_record_all_long %>% 
  ungroup() %>% 
  dplyr::select(tribe,FIPS) %>% 
  unique() 

allcounties <- tigris::counties() %>% 
  st_as_sf() %>% 
  dplyr::select(GEOID) %>% 
  right_join(.,tribecounties,by=c("GEOID" = "FIPS"))

tribedf <- write_rds(allcounties,"01_data/cache/tribe_county_shapefiles.rds")
  
  