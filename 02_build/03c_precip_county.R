
# from https://prism.oregonstate.edu

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

prism <- raster("01_data/PRISM_ppt_30yr_normal_4kmM2_annual_asc/PRISM_ppt_30yr_normal_4kmM2_annual_asc.asc")

slicecoords <- tibble(lon = xyFromCell(prism, 1:ncell(prism))[,1],
                      lat = xyFromCell(prism, 1:ncell(prism))[,2]) %>%
  rownames_to_column(var = "cell") %>% 
  mutate(cell = as.numeric(cell))

extracted <- terra::extract(prism,tribeuse,
                            weights = T, 
                            normalizeWeights = T,
                            cellnumbers = T)


e1 <- extracted[1]
GEOID <- tribedf$GEOID[1]

precip <- map2_dfr(extracted,tribeuse$GEOID,function(e1,GEOID){
  
  e1dt <- e1 %>% 
    as.data.table()
  
  if (nrow(e1dt) > 0){
    
    temp <- e1dt %>% 
      merge(., slicecoords, all.x=T, by="cell") %>%
      .[, c("precip","GEOID") :=
          .(value*weight,GEOID)] %>% 
      group_by(GEOID) %>% 
      summarise(precip = sum(precip)) %>% 
      ungroup()
    
  } 
})

write_rds(precip,"01_data/clean/c_precip_county.rds")
