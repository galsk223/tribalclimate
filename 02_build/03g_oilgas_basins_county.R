
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

tribetotalarea <- tribeuse %>%
  mutate(GEOIDarea = st_area(geometry)) %>% 
  st_transform(4269)



basins <- read_sf("01_data/SedimentaryBasins_US_EIA/SedimentaryBasins_US_May2011_v2.shp") %>% 
  st_transform(4269)

tribe_basins <- st_intersection(basins,tribetotalarea) %>% 
  mutate(BasinArea = st_area(geometry),
         BasinPortion = BasinArea/GEOIDarea) %>% 
  st_set_geometry(NULL) %>% 
  group_by(GEOID) %>% 
  summarise(BasinPortion = as.numeric(sum(BasinArea)/sum(GEOIDarea))) %>% 
  ungroup() %>% 
  left_join(tribeuse, ., by = "GEOID") %>% 
  replace_na(list(BasinPortion = 0)) %>% 
  mutate(BasinBinary = ifelse(BasinPortion > 0, 1, 0))

write_rds(tribe_basins,"01_data/clean/g_OilGas_basins_county.rds")
