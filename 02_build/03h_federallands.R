# for federal lands, filtered to manager type == federal
# see doc in folder for more info


rm(list = ls())

tribedf <- read_rds("01_data/cache/tribe_shapefiles.rds") 

tribeunqiue <- tribedf %>% 
  st_set_geometry(NULL) %>% 
  distinct(UID)

tribebuffed <- tribedf %>% 
  st_transform(5070) %>% 
  st_buffer(160.934) %>% 
  mutate(BuffedArea = st_area(geometry))

regions <- 1:10
r <- 1

pad <- map_dfr(regions,function(r){
  
  temp <- read_sf(paste0("01_data/PAD/PADUS2_1_Region",r,"_Shapefile/",
                         "PADUS2_1Combined_Tribal_DOD_Fee_Designation_Easement_Region",r,".shp")) %>% 
    filter(Mang_Type == "FED")
  
  PADtribe <- tribebuffed %>% 
    st_intersection(st_buffer(temp,0),.) %>% 
    mutate(PADArea = st_area(geometry),
           PADPortion = PADArea/BuffedArea) %>% 
    st_set_geometry(NULL) %>% 
    dplyr::select(UID,GEOID,AIANNHCE,tribe_name,BuffedArea,PADArea,PADPortion)
  
  print(r)
  return(PADtribe)
  
})

padsave <- pad %>% 
  group_by(UID) %>% 
  summarise(PADPortion = sum(PADArea)/sum(BuffedArea)) %>% 
  ungroup() %>% 
  left_join(tribeunqiue,.,by="UID") %>% 
  replace_na(list(PADPortion = 0))

write_rds(padsave,"01_data/clean/h_federalland.rds")  