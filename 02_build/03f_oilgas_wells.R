
# CODING
## 1 - cell contains >= 1 oil, no gas 
## 2 - cell contains >= 1 gas, no oil
## 3 - cell contains >= 1 oil AND >= 1 gas 
## 4 - mystery wells

rm(list = ls())
fl1 <- list.files("01_data/OilGas_RawData/uscells06g_timeslices/shape", full.names = T)

tribedf <- read_rds("01_data/cache/tribe_shapefiles.rds") %>% 
  st_transform(4269)

dec <- c("pre1900",paste0(seq(1900,2000,by=10),"s"))
d <- dec[11]

wells <- map_dfr(dec,function(d){
  
  raw <- read_sf(paste0("01_data/OilGas_RawData/uscells06g_timeslices/shape/",d,"_1sqmicg.shp")) %>% 
    st_transform(4269)
  
  intribes <- st_intersection(st_buffer(raw,0),tribedf) %>% 
    st_set_geometry(NULL) %>% 
    mutate(Decade = d)
  
  return(intribes)
  
})

tosave <- wells %>% 
  group_by(UID,Decade) %>% 
  tally() %>% 
  ungroup()

write_rds(tosave,"01_data/clean/f_OilGas_wells.rds")
