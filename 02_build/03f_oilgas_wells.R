
# CODING
## 1 - cell contains >= 1 oil, no gas 
## 2 - cell contains >= 1 gas, no oil
## 3 - cell contains >= 1 oil AND >= 1 gas 
## 4 - mystery wells

rm(list = ls())
fl1 <- list.files("01_data/OilGas_RawData/uscells06g_timeslices/shape", full.names = T)

tribedf <- read_rds("01_data/cache/tribe_shapefiles.rds") %>% 
  st_transform(4269)

tribetotalarea <- tribedf %>% 
  st_set_geometry(NULL) %>% 
  distinct(UID,UIDarea)


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

intribes_oil_df <- st_intersection(st_buffer(wells_oil,0),tribedf)

actUID <- unique(intribes_oil_df$UID)
aID <- actUID[1]

intribes_oil_unioned <- map_dfr(actUID, function(aID){
  
  temp <- intribes_oil_df %>% 
    filter(UID == aID) %>% 
    dplyr::select(geometry) %>% 
    unique() %>%
    summarize(geometry = st_union(geometry)) %>% 
    mutate(AllArea_Oil = st_area(geometry),
           UID = aID) %>% 
    st_set_geometry(NULL) %>% 
    left_join(., tribetotalarea, by=c("UID")) %>% 
    mutate(AllArea_OilPortion = AllArea_Oil/UIDarea) 
  
  print(match(aID,actUID))
  return(temp)
  
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


intribes_gas_df <- st_intersection(st_buffer(wells_gas,0),tribedf)

actUID <- unique(intribes_gas_df$UID)
aID <- actUID[1]

intribes_gas_unioned <- map_dfr(actUID, function(aID){
  
  temp <- intribes_gas_df %>% 
    filter(UID == aID) %>% 
    dplyr::select(geometry) %>% 
    unique() %>%
    summarize(geometry = st_union(geometry)) %>% 
    mutate(AllArea_Gas = st_area(geometry),
           UID = aID) %>% 
    st_set_geometry(NULL) %>% 
    left_join(., tribetotalarea, by=c("UID")) %>% 
    mutate(AllArea_GasPortion = AllArea_Gas/UIDarea) 
  
  print(paste0(match(aID,actUID)," / ",length(actUID)))
  return(temp)
  
})



# -----------------------------------
# save
# -----------------------------------

write_rds(intribes_oil_unioned,"01_data/clean/f_Oil_wells.rds")
write_rds(intribes_gas_unioned,"01_data/clean/f_Gas_wells.rds")




