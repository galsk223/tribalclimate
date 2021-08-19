

# from https://www.arcgis.com/home/item.html?id=680e87c5b1d34e0585203aa4f67d8426

rm(list = ls())
tribedf <- read_rds("01_data/cache/tribe_shapefiles.rds")

if(!dir.exists("01_data/cache/heat_gridmet")){
  dir.create("01_data/cache/heat_gridmet")
}


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
bridge <- read_rds("01_data/cache/bridge_pdsi.rds")


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
    pivot_longer(-c(UID,lon,lat,weight),
                 names_to = "date",
                 values_to = "value") %>% 
    group_by(UID,lon,lat,weight) %>% 
    summarise(heatdays = sum(value > 310.928, na.rm = T)) %>% 
    ungroup() %>% 
    group_by(UID) %>% 
    summarise(heatdays_year = mean(heatdays, na.rm = T)) %>% 
    ungroup() 
  
  write_rds(var,paste0("01_data/cache/heat_gridmet/",str_sub(heat1,-7,-4),".rds"))
  print(heat1)

})

cached <- list.files("01_data/cache/heat_gridmet", full.names = T)
heat_all <- map_dfr(cached,function(c){
  
  temp <- read_rds(c) %>% 
    mutate(Year = str_sub(c,-8,-5))
  
})

heat_save <- heat_all %>% 
  group_by(UID) %>% 
  summarise(heatdays_mean = mean(heatdays_year, na.rm = T)) %>% 
  ungroup() 

write_rds(heat_save,"01_data/clean/a_heat_gridmet.rds")
  



