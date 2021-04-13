# for federal lands, filtered to manager type == federal
# see doc in folder for more info


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
  st_set_geometry(NULL)

tribeusebuffed <- tribeuse %>% 
  st_transform(5070) %>% 
  st_buffer(160.934) %>% 
  mutate(BuffedArea = st_area(geometry))
  
  
  

regions <- 1:10
r <- 1

pad <- map_dfr(regions,function(r){
  
  temp <- read_sf(paste0("01_data/PAD_raw/PADUS2_1_Region",r,"_Shapefile/",
                         "PADUS2_1Combined_Tribal_DOD_Fee_Designation_Easement_Region",r,".shp")) %>% 
    filter(Mang_Type == "FED")
  
  PADtribe <- tribeusebuffed %>% 
    st_intersection(st_buffer(temp,0),.) %>% 
    mutate(PADArea = st_area(geometry),
           PADPortion = PADArea/BuffedArea) %>% 
    st_set_geometry(NULL) %>% 
    dplyr::select(GEOID,BuffedArea,PADArea,PADPortion)
  
  print(r)
  return(PADtribe)
  
})

padsave <- pad %>% 
  group_by(GEOID) %>% 
  summarise(PADPortion = as.numeric(sum(PADArea)/sum(BuffedArea))) %>% 
  ungroup() %>% 
  left_join(tribeuse,.,by="GEOID") %>% 
  st_set_geometry(NULL) %>% 
  replace_na(list(PADPortion = 0))

write_rds(padsave,"01_data/clean/h_federalland_county.rds")  
