

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

#Access data in Arc grid file
whp <- raster("01_data/RDS-2015-0046-2/Data/whp_2018_classified/whp2018_cls/w001001.adf")

slicecoords <- tibble(lon = xyFromCell(whp, 1:ncell(whp))[,1],
                      lat = xyFromCell(whp, 1:ncell(whp))[,2]) %>%
  rownames_to_column(var = "cell") %>% 
  mutate(cell = as.numeric(cell))

tribe1 <- tribeuse %>% 
  mutate(State = str_sub(GEOID,1,2)) %>% 
  group_split(State)

gc()
extracted <- map_dfr(tribe1, function(t1){
  
  ext <- terra::extract(whp,t1)
  
  write_rds(ext, paste0("01_data/cache/extracted_county/03d_",unique(t1$num),".rds"))
  return(ext)
  
})

ex_files <- str_subset(list.files("01_data/cache/extracted_county", full.names = T),"03d")

ef <- ex_files[1]

extracted <- map_dfr(ex_files,function(ef){
  
  r <- read_rds(ef)
  
  output <- map(r,function(x){
    y <- ifelse(x>=6,0,x)
    out <- tibble(whp_mean=mean(y,na.rm=T),
                  whp_q25=quantile(y,p=.25,na.rm=T),
                  whp_median=quantile(y,p=.5,na.rm=T),
                  whp_q75=quantile(y,p=.75,na.rm=T))
    return(out)
  }) 
  
  all <- reduce(output, bind_rows)
  
})

ex_save <- bind_cols(tribeuse,extracted) %>% 
  st_set_geometry(NULL) 

write_rds(ex_save,"01_data/clean/d_whp_county.rds")




