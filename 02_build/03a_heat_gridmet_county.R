

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
  filter(!GEOID %in% empty$GEOID) %>% 
  mutate(State = str_sub(GEOID,1,2)) %>% 
  group_split(State)


# download gridmet
file.list <- expand.grid("tmmx",1979:2001,stringsAsFactors = F) %>% 
  rename(var=Var1,year=Var2) %>%
  mutate(var=str_to_lower(var),
         file.name = str_c(var,"_",year,".nc")) %>% 
  arrange(var) 

map(1:dim(file.list)[1],function(f){
  
  target.file <- str_c("01_data/tmmx/",file.list$file.name[f])
  
  if (file.exists(target.file)){
    message("Target file exists")
  } else {
    #Download the file
    download(url=str_c("http://www.northwestknowledge.net/metdata/data/",file.list$file.name[f]),
             destfile = target.file,
             mode = 'wb')
  }
  
})

# using bridge from PDSI (same raster dimensions)
pdsi <- raster("01_data/pdsi.nc")

slicecoords <- tibble(lon = xyFromCell(pdsi[[1]], 1:ncell(pdsi[[1]]))[,1],
                      lat = xyFromCell(pdsi[[1]], 1:ncell(pdsi[[1]]))[,2]) %>%
  rownames_to_column(var = "cell") %>%
  mutate(cell = as.numeric(cell))

extall <- map(str_subset(
  list.files("01_data/cache/extracted_county", full.names = T),"03b"),function(f){
    
    r <- read_rds(f)
    
})

n <- 3

bridge <- map_dfr(1:48,function(n){
  
  e1 <- extall[n]
  t1 <- tribeuse[n] %>% 
    as.data.table()
  
  r <- map_dfr(1:length(e1[[1]]),function(e2){
    
    e2dt <- e1[[1]][e2] %>% 
      as.data.table()
    
    if (nrow(e2dt) > 0){
      
      temp <- e2dt %>% 
        merge(., slicecoords, all.x=T, by="cell") %>% 
        dplyr::select(lon,lat,weight) %>% 
        mutate(GEOID = t1$GEOID[e2])
      
    }
    
    return(temp)
  }) 
  
  print(n)
  return(r)
})

write_rds(bridge,"01_data/cache/bridge_heat_county.rds")
bridge <- read_rds("01_data/cache/bridge_heat_county.rds")


# -----------------------------------
# extract and roll up
# -----------------------------------
allheat <- paste0("01_data/tmmx/",file.list$file.name)
heat1 <- allheat[length(allheat)]

map(allheat, function(heat1){
  
  nc <- nc_open(heat1)
  var.id=names(nc$var)
  date.vector <- as_date(nc$dim$day$vals,origin="1900-01-01")
  
  nc.data <- ncvar_get(nc = nc, varid = var.id)[,,]
  
  nc.data.df <- array(nc.data,dim=c(prod(dim(nc.data)[1:2]),dim(nc.data)[3])) %>%
    as_tibble(.name_repair = "universal") %>%
    rename_all(~str_c(date.vector))
  
  nc_lat <- ncvar_get(nc = nc, varid = "lat")
  nc_lon <- ncvar_get(nc = nc, varid = "lon")
  nc.coords <- expand.grid(lon=nc_lon,lat=nc_lat) %>% 
    mutate(lon = round(lon,3),
           lat = round(lat,3))
  
  nc.df <- bind_cols(nc.coords,nc.data.df)
  

  
  var <- bridge %>% 
    mutate(lat = round(lat,3),
           lon = round(lon,3)) %>% 
    inner_join(.,
               nc.df,
               by=c("lat","lon")) %>%
    pivot_longer(-c(GEOID,lon,lat,weight),
                 names_to = "date",
                 values_to = "value") %>% 
    group_by(GEOID,lon,lat,weight) %>% 
    summarise(heatdays = sum(value > 310.928, na.rm = T)) %>% 
    ungroup() %>% 
    group_by(GEOID) %>% 
    summarise(heatdays_year = mean(heatdays, na.rm = T)) %>% 
    ungroup() 
  
  write_rds(var,paste0("01_data/cache/heat_gridmet_county/",str_sub(heat1,-7,-4),".rds"))
  print(heat1)

})

cached <- list.files("01_data/cache/heat_gridmet_county", full.names = T)
heat_all <- map_dfr(cached,function(c){
  
  temp <- read_rds(c) %>% 
    mutate(Year = str_sub(c,-8,-5))
  
})

heat_save <- heat_all %>% 
  group_by(GEOID) %>% 
  summarise(heatdays_mean = mean(heatdays_year, na.rm = T)) %>% 
  ungroup() 

write_rds(heat_save,"01_data/clean/a_heat_gridmet_county.rds")
  



