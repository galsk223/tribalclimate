
rm(list = ls())
tribedf <- read_rds("01_data/cache/tribe_shapefiles.rds") %>% 
  st_transform(5070)


if(!dir.exists("01_data/cache/elevationruggedness")){
  dir.create("01_data/cache/elevationruggedness")
}



# t <- unique(tribedf$UID)[1]

#Begin loop over counties
done <- tibble(files = list.files("01_data/cache/elevationruggedness/")) %>% 
  mutate(UID = str_sub(files,1,-5))

tribedo <- tribedf %>% 
  filter(!UID %in% done$UID)

map(unique(tribedo$UID),function(t){
  
  tribenow <- tribedf %>% 
    filter(UID == t)
  
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
    mutate(UID = t)
  
  write_rds(out, paste0("01_data/cache/elevationruggedness/",t,".rds"))
  return(out)
  
}) 



fl <- list.files("01_data/cache/elevationruggedness/", full.names = T)

wire.save <- map_dfr(fl,function(f){
  
  r <- read_rds(f)
  
})

write_rds(wire.save,"01_data/clean/e_ElevationAndRuggedness.rds")













