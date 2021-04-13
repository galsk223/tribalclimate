
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
  filter(!GEOID %in% empty$GEOID) %>% 
  st_transform(5070)

# t <- unique(tribedf$UID)[1]

#Begin loop over counties
done <- tibble(files = list.files("01_data/cache/elevationruggedness_county/")) %>% 
  mutate(GEOID = str_sub(files,1,-5))

if(nrow(done)==0){
  tribedo <- tribeuse
} else {
  tribedo <- tribeuse %>% 
    filter(!GEOID %in% done$GEOID)
}

map(unique(tribedo$GEOID),function(t){
  
  tribenow <- tribeuse %>% 
    filter(GEOID == t)
  
  #Generate DEM for county
  elev.raster <- get_elev_raster(tribenow,z=9,expand = 1)
  names(elev.raster) <- "elevation"
  
  #Calculating topographic information and appending to elevation
  topo.raster <- stack(elev.raster,terrain(elev.raster,opt = c("slope","aspect","TRI")))
  
  #Extract elevation data
  elev.temp <- extract(topo.raster,
                       tribenow)
  
  out <- map_dfr(1:length(elev.temp),function(et){
    
    r <- as_tibble(elev.temp[[et]])

  }) %>% 
    summarize_all(funs(mean=mean(.,na.rm=T),
                       sd=sd(.,na.rm=T),
                       min=min(.,na.rm=T),
                       q25=quantile(.,p=.25,na.rm=T),
                       median=quantile(.,p=.5,na.rm=T),
                       q75=quantile(.,p=.25,na.rm=T),
                       max=max(.,na.rm=T)))  %>% 
    mutate(GEOID = t)
  
  write_rds(out, paste0("01_data/cache/elevationruggedness_county/",t,".rds"))
  return(out)
  
}) 



fl <- list.files("01_data/cache/elevationruggedness_county/", full.names = T)

wire.save <- map_dfr(fl,function(f){
  
  r <- read_rds(f)
  
})

write_rds(wire.save,"01_data/clean/e_ElevationAndRuggedness_County.rds")













