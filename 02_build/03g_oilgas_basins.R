
rm(list = ls())

tribedf <- read_rds("01_data/cache/tribe_shapefiles.rds") 
tribeunqiue <- tribedf %>% 
  st_set_geometry(NULL) %>% 
  distinct(UID)


basins <- read_sf("01_data/SedimentaryBasins_US_EIA/SedimentaryBasins_US_May2011_v2.shp") %>% 
  st_transform(4269)

tribe_basins <- st_intersection(basins,tribedf) %>% 
  mutate(BasinArea = st_area(geometry),
         BasinPortion = BasinArea/area) %>% 
  st_set_geometry(NULL) %>% 
  group_by(UID) %>% 
  summarise(BasinPortion = as.numeric(sum(BasinArea)/sum(area))) %>% 
  ungroup() %>% 
  left_join(tribeunqiue, ., by = "UID") %>% 
  replace_na(list(BasinPortion = 0)) %>% 
  mutate(BasinBinary = ifelse(BasinPortion > 0, 1, 0))

write_rds(tribe_basins,"01_data/clean/g_OilGas_basins.rds")
