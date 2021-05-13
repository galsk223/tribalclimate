
# from https://prism.oregonstate.edu

rm(list = ls())
tribedf <- read_rds("01_data/cache/tribe_shapefiles.rds")

prism <- raster("01_data/PRISM_ppt_30yr_normal_4kmM2_annual_asc/PRISM_ppt_30yr_normal_4kmM2_annual_asc.asc")

slicecoords <- tibble(lon = xyFromCell(prism, 1:ncell(prism))[,1],
                      lat = xyFromCell(prism, 1:ncell(prism))[,2]) %>%
  rownames_to_column(var = "cell") %>% 
  mutate(cell = as.numeric(cell))

extracted <- terra::extract(prism,tribedf,
                            weights = T, 
                            normalizeWeights = T,
                            cellnumbers = T)


# e1 <- extracted[1]
# UID <- tribedf$UID[1]

precip <- map2_dfr(extracted,tribedf$UID,function(e1,UID){
  
  e1dt <- e1 %>% 
    as.data.table()
  
  if (nrow(e1dt) > 0){
    
    temp <- e1dt %>% 
      merge(., slicecoords, all.x=T, by="cell") %>%
      .[, c("precip","UID") :=
          .(value*weight,UID)] %>% 
      group_by(UID) %>% 
      summarise(precip = sum(precip, na.rm = T)) %>% 
      ungroup()
    
  } 
})

precipout <- precip %>% 
  left_join(tribedf,.,by="UID") %>% 
  st_set_geometry(NULL) %>% 
  mutate(precipweight = precip*area_weighted, na.rm = T) %>% 
  group_by(UID) %>% 
  summarise(precip = mean(precipweight, na.rm = T)) %>% 
  ungroup()

write_rds(precipout,"01_data/clean/c_precip.rds")
