
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


# -----------------------------------
# prep
# -----------------------------------

pdsi <- raster("01_data/pdsi.nc")

slicecoords <- tibble(lon = xyFromCell(pdsi[[1]], 1:ncell(pdsi[[1]]))[,1],
                      lat = xyFromCell(pdsi[[1]], 1:ncell(pdsi[[1]]))[,2]) %>%
  rownames_to_column(var = "cell") %>%
  mutate(cell = as.numeric(cell))

# 2 and a half hours
extracted <- map_dfr(1:length(tribeuse),function(st){
  
  ext <- terra::extract(pdsi[[1]],tribeuse[[st]],
                 weights = T,
                 normalizeWeights = T,
                 cellnumbers = T)
  
  write_rds(ext, paste0("01_data/cache/extracted_county/03b_",st,".rds"))
  return(ext)
  
})

extall <- map(str_subset(
  list.files("01_data/cache/extracted_county", full.names = T),"03b"),function(f){
    
    r <- read_rds(f)
    
  })
  
  
e1 <- extall[3]
t1 <- tribeuse[3]
e2 <- 10
n <- 1 

bridge.pdsi <- map_dfr(1:48,function(n){
  
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
    print(e2)
    return(temp)
  }) 
  return(r)
})

# bridge.use <- reduce(bridge.pdsi, bind_rows)



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
         lon = round(lon,3)) %>% 
  group_split(GEOID)

bj <- bridge.join[[1]]

var.cbsa <- map_dfr(bridge.join,function(bj){
  
  t <- inner_join(bj,
             nc.df,
             by=c("lat","lon")) %>%
    pivot_longer(-c(lon,lat,weight,GEOID),
                 names_to = "date",
                 values_to = "value")  %>% 
    group_by(GEOID,date) %>% 
    summarise(totalweight = sum(weight, na.rm = T),
              weekPDSI = sum(value*weight/totalweight, na.rm = T)) %>% 
    ungroup() %>% 
    group_by(GEOID) %>% 
    summarise("1980-2020 Mean" = mean(weekPDSI)) %>% 
    ungroup()
  
})
  
  
write_rds(var.cbsa,"01_data/clean/b_drought_county.rds")


