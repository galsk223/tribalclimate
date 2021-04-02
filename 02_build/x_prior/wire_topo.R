#Script to read in gridded digital elevation model

##########################
#Retrieving raster elevation data from elevatr
#Break up communities by county, then grab raster layer 
#and process for each county, then rbind

county.names <- unique(wire.comm$County) 

#Define county subsets for cutting structures data
counties <- us_counties(resolution = "low",states = c("Colorado","Washington")) %>% 
  filter(name %in% county.names) %>% 
  select(geoid,name,state_abbr) %>%
  st_transform(5070)

#Define progress bar
pb <- progress_estimated(dim(wire.comm)[1])

#Begin loop over counties
wire.topo <- map(1:length(county.names),function(c){
  
  #Generate DEM for county
  elev.raster <- get_elev_raster(counties %>% filter(name==county.names[c]),z=9,expand = 1)
  names(elev.raster) <- "elevation"
  
  #Calculating topographic information and appending to elevation
  topo.raster <- stack(elev.raster,terrain(elev.raster,opt = c("slope","aspect","TRI")))
  
  #Subsetting communities by county
  wire.temp <- wire.comm %>% 
    filter(County==county.names[c]) %>%
    select(Community,County)
  
  #Extract elevation data
  elev.temp <- extract(topo.raster,
                       wire.temp)
  
  #Calculating elevation stats for each community
  elev.output <- map(elev.temp,function(y){
    pb$tick()$print()
    out <- as_tibble(y) %>%
      summarize_all(funs(mean=mean(.,na.rm=T),
                         sd=sd(.,na.rm=T),
                         min=min(.,na.rm=T),
                         q25=quantile(.,p=.25,na.rm=T),
                         median=quantile(.,p=.5,na.rm=T),
                         q75=quantile(.,p=.25,na.rm=T),
                         max=max(.,na.rm=T)))
    return(out)
  }) %>% 
    bind_rows() %>%
    bind_cols(wire.temp %>% st_set_geometry(NULL),.)
}) %>% 
  bind_rows() %>%
  select(-County)



save(wire.topo,file = "cache/wire_topo.Rdata")  





