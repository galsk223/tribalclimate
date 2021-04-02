
rm(list = ls())
tribedf <- read_rds("01_data/cache/tribe_shapefiles.rds")

# download(url=str_c("http://www.northwestknowledge.net/metdata/data/pdsi.nc"),
#          destfile = "01_data/pdsi.nc",
#          mode = 'wb')


# -----------------------------------
# prep
# -----------------------------------

pdsi <- raster("01_data/pdsi.nc")

slicecoords <- tibble(lon = xyFromCell(pdsi[[1]], 1:ncell(pdsi[[1]]))[,1],
                      lat = xyFromCell(pdsi[[1]], 1:ncell(pdsi[[1]]))[,2]) %>%
  rownames_to_column(var = "cell") %>%
  mutate(cell = as.numeric(cell))

extracted <- terra::extract(pdsi[[1]],tribedf,
                            weights = T,
                            normalizeWeights = T,
                            cellnumbers = T)

# e1 <- extracted[1]
# UID <- tribedf$UID[1]

bridge.pdsi <- map2_dfr(extracted,tribedf$UID,function(e1,UID){
  
  e1dt <- e1 %>% 
    as.data.table()
  
  if (nrow(e1dt) > 0){
    
    temp <- e1dt %>% 
      merge(., slicecoords, all.x=T, by="cell") %>% 
      dplyr::select(lon,lat,weight) %>% 
      mutate(UID = UID)
    
  } 
})

heatout <- heat %>% 
  left_join(tribedf,.,by="UID") %>% 
  st_set_geometry(NULL) %>% 
  mutate(heatweight = heatdays*area_weighted, na.rm = T) %>% 
  group_by(UID) %>% 
  summarise(heatdays = sum(heatweight)) %>% 
  ungroup()



# -----------------------------------
# extract and roll up
# -----------------------------------

nc <- nc_open("01_data/pdsi.nc")
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



bridge.join <- bridge.pdsi %>% 
  mutate(lat = round(lat,3),
         lon = round(lon,3))

var.cbsa <- inner_join(bridge.join,
                       nc.df,
                       by=c("lat","lon")) %>%
  pivot_longer(-c(lon,lat,weight,UID),
               names_to = "date",
               values_to = "value")  %>% 
  group_by(UID,date) %>% 
  summarise(weekPDSI = sum(value*weight)) %>% 
  ungroup() %>% 
  group_by(UID) %>% 
  summarise("1980-2020 Mean" = mean(weekPDSI)) %>% 
  ungroup()

write_rds(var.cbsa,"01_data/clean/b_drought.rds")


