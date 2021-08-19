library(tidyverse)
library(data.table)
library(janitor)
library(sf)
library(mapview)

rm(list = ls())
tribe_df <- read_rds("01_data/cache/tribe_bg_file_names.rds") %>%
  separate(file_path,into = c("d1","d2","rt","tribe","file"),sep = "/") 



# -----------------------------------
# for air_federal, air_state, otsa, sdtsa, tdsa
#  FIPS found in census native areas, confirmed in documentation:
#  https://www2.census.gov/geo/pdfs/maps-data/data/tiger/tgrshp2018/TGRSHP2018_TechDoc_Ch3.pdf
# for air_federal, the polygons are grouped tribal block groups,
#  matching the the polygons from census tribal block groups shapefile (below)
# -----------------------------------

fedstatedesig <- tribe_df %>% 
  filter(rt %in% c("air_state", "air_federal", "otsa", "sdtsa", "tdsa")) %>% 
  mutate(tribe_code = str_extract(tribe,"\\d+"),
         tribe_name = str_extract(tribe,"(?<=_).+")) %>% 
  dplyr::select(tribe_code,tribe_name) %>% 
  unique() 

t2 <- tigris::native_areas(class = "sf") %>% 
  dplyr::select(AIANNHCE,
         GEOID) %>% 
  inner_join(., fedstatedesig, by = c("AIANNHCE" = "tribe_code")) %>% 
  mutate(UID = AIANNHCE)


# -----------------------------------
# for tribal subdivisions
#  GEOID found in census tribal areas, confirmed in documentation:
#  https://catalog.data.gov/dataset/tiger-line-shapefile-2017-nation-u-s-current-american-indian-tribal-subdivision-aits-national
# -----------------------------------

tribalsub <- tribe_df %>% 
  filter(rt %in% c("tribalsub")) %>% 
  mutate(tribe_code = str_extract(tribe,"\\d+"),
         tribe_name = str_sub(tribe,11,-1)) %>% 
  dplyr::select(tribe_code,tribe_name) %>% 
  unique() 

t3 <- read_sf("01_data/tl_2017_us_aitsn/tl_2017_us_aitsn.shp") %>% 
  dplyr::select(AIANNHCE,
         GEOID) %>% 
  inner_join(., tribalsub, by = c("GEOID" = "tribe_code")) %>% 
  mutate(UID = GEOID)

# -----------------------------------
# for remaining 10 tribal subdivisions
# -----------------------------------

missing <- tribe_df %>% 
  mutate(tribe_code = str_extract(tribe,"\\d+"),
         tribe_name = str_sub(tribe,11,-1),
         geoid10_f = paste0(state,county,
                            str_pad(str_remove(tract,"\\."), width = 6, side = "left", pad = "0"),
                            block)) %>% 
  filter(rt %in% c("tribalsub"),
         !tribe_code %in% t3$UID) 

mc <- unique(missing$county)[1]
mb <- map(unique(missing$county),function(mc){
  
  missingblocks <- tigris::blocks(state = 40,
                                  county = mc)
  
  mbsf <- missingblocks %>% 
    st_as_sf() %>% 
    filter(GEOID10 %in% 
             filter(missing, 
                    county == mc)$geoid10_f) %>% 
    dplyr::select(GEOID10)
  
})

mb1 <- bind_rows(mb[[1]],mb[[2]],mb[[3]],mb[[4]]) %>% 
  left_join(missing,.,by=c("geoid10_f" = "GEOID10")) %>% 
  dplyr::select(GEOID = geoid10_f, geometry,
         tribe_name, UID = tribe_code) %>% 
  mutate(AIANNHCE = str_sub(GEOID,1,4))


# -----------------------------------
# combine and save
# -----------------------------------

tall <- bind_rows(t2,t3,mb1) %>% 
  mutate(area = st_area(geometry))

tarea <- tall %>% 
  st_set_geometry(NULL) %>% 
  group_by(UID) %>% 
  summarise(UIDarea = sum(area)) %>% 
  ungroup()

tareaw <- tall %>% 
  left_join(.,tarea, by = "UID") %>% 
  mutate(area_weighted = as.numeric(area/UIDarea)) 
  
write_rds(tareaw,"01_data/cache/tribe_shapefiles.rds")
# write_rds(tareaw,"/RSTOR/cache/tribe_shapefiles.rds")









# ----------------------------------------------------------------------
# ----------------------------------------------------------------------
# CAN IGNORE
# -----------------------------------
# for air_federal 
#  using tribal block groups to match to census shapefiles
# -----------------------------------
# 
# air_fed <- tribe_df %>% 
#   filter(rt == "air_federal") %>% 
#   mutate(tribe_code = str_extract(tribe,"\\d+"),
#          tribe_name = str_sub(tribe,7,-1)) %>% 
#   select(tribe_code,tribe_name) %>% 
#   unique()
# 
# t1 <- tigris::tribal_block_groups(class = "sf") %>% 
#   select(AIANNHCE,
#          Tribal_Tract = TTRACTCE,
#          Tribal_BlockGroup = TBLKGPCE,
#          GEOID) %>% 
#   inner_join(., air_fed, by = c("AIANNHCE" = "tribe_code"))
# 
# t1b <- tigris::native_areas(class = "sf") %>% 
#   select(AIANNHCE,
#          GEOID) %>% 
#   inner_join(., air_fed, by = c("AIANNHCE" = "tribe_code")) 

