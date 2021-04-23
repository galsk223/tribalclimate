

# from https://www.arcgis.com/home/item.html?id=680e87c5b1d34e0585203aa4f67d8426

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

heatraster <- raster("01_data/HeatStressIndex/HeatStressIndex/HSI_historical.tif")                                   

slicecoords <- tibble(lon = xyFromCell(heatraster, 1:ncell(heatraster))[,1],
                      lat = xyFromCell(heatraster, 1:ncell(heatraster))[,2]) %>%
  rownames_to_column(var = "cell") %>% 
  mutate(cell = as.numeric(cell))

extracted <- terra::extract(heatraster,tribeuse,
                            weights = T, 
                            normalizeWeights = T,
                            cellnumbers = T)


e1 <- extracted[1]
UID <- tribedf$UID[1]

heat <- map2_dfr(extracted,tribeuse$GEOID,function(e1,GEOID){
  
  e1dt <- e1 %>% 
    as.data.table()
  
  if (nrow(e1dt) > 0){
    
    temp <- e1dt %>% 
      merge(., slicecoords, all.x=T, by="cell") %>%
      .[, c("heatdays","GEOID") :=
          .(value*weight,GEOID)] %>% 
      group_by(GEOID) %>% 
      summarise(heatdays = sum(heatdays, na.rm = T)) %>% 
      ungroup()
    
  } 
})

heatout <- heat %>% 
  left_join(tribedf,.,by="GEOID") %>% 
  st_set_geometry(NULL) %>% 
  group_by(GEOID) %>% 
  summarise(heatdays = mean(heatdays)) %>% 
  ungroup()

write_rds(heatout,"01_data/clean/a_heat_county.rds")
h <- read_rds("01_data/clean/a_heat.rds")
