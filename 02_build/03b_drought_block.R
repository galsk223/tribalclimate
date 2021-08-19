# terminal command for screen

setwd("/home/sambk223/tribalclimate")

library(tidyverse)
library(data.table)
library(conflicted)
library(lubridate)
library(sf)
library(tigris)
library(RPostgreSQL)
library(dbplyr)
library(RcppRoll)
library(scales)
library(broom)
library(dotwhisker)
library(janitor)
library(directlabels)
library(googledrive)
library(cowplot)
library(plotly)
library(rmarkdown)
library(readxl)
library(SafeGraphR)
library(vroom)
library(ncdf4)

conflict_prefer("filter", "dplyr")
conflict_prefer("between", "dplyr")
conflict_prefer("last", "dplyr")
conflict_prefer("lag", "dplyr")
conflict_prefer("year", "lubridate")
conflict_prefer("ggsave", "ggplot2")

Sys.setenv(RSTUDIO_PANDOC="/usr/lib/rstudio-server/bin/pandoc")

rm(list = ls())

tribedf <- read_rds("01_data/cache/tribe_shapefiles_micro.rds")

if(!dir.exists("01_data/cache/bridge_cache")){
  dir.create("01_data/cache/bridge_cache")
}
if(!dir.exists("01_data/clean/b_drought_blocks")){
  dir.create("01_data/clean/b_drought_blocks")
}
# download(url=str_c("http://www.northwestknowledge.net/metdata/data/pdsi.nc"),
#          destfile = "01_data/pdsi.nc",
#          mode = 'wb')


# -----------------------------------
# prep
# -----------------------------------

done <- as.numeric(str_sub(list.files("01_data/cache/bridge_cache/"),1,-4))
todo <- setdiff(1:nrow(tribedf),done)
tb <- todo[1]

if(length(todo) > 152){
  
  pdsi <- raster("01_data/pdsi.nc")
  
  slicecoords <- tibble(lon = xyFromCell(pdsi[[1]], 1:ncell(pdsi[[1]]))[,1],
                        lat = xyFromCell(pdsi[[1]], 1:ncell(pdsi[[1]]))[,2]) %>%
    rownames_to_column(var = "cell") %>%
    mutate(cell = as.numeric(cell))
  
  gc()
  bridge.county <- future_map(todo,function(tb){
    
    extracted <- terra::extract(pdsi[[1]],tribedf[tb,],
                                weights = T,
                                normalizeWeights = T,
                                cellnumbers = T)
    
    e1dt <- extracted[[1]] %>% 
      as.data.table()
    
    if (nrow(e1dt) > 0){
      
      temp <- e1dt %>% 
        merge(., slicecoords, all.x=T, by="cell") %>% 
        dplyr::select(lon,lat,weight) %>% 
        bind_cols(tribedf[tb,],.)
      
      write_rds(temp,paste0("01_data/cache/bridge_cache/",tb,".rds"))
      
    } 
    
    print(tb)
    
  })
}




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


fl <- tibble(fl = list.files("01_data/cache/bridge_cache", full.names = T)) %>% 
  add_rownames() %>% 
  mutate(set = floor(as.numeric(rowname)/1000)) %>% 
  group_split(set)

fs <- fl[[1]]

map(fl,function(fs){
  
  bridge.join <- map_dfr(fs$fl,function(f_in){
    r <- read_rds(f_in) %>% 
      st_set_geometry(NULL) %>% 
      dplyr::select(GEOID10,tribe,lon,lat,weight) %>% 
      mutate(lat = round(lat,3),
             lon = round(lon,3))
  })
  
  var.block <- inner_join(bridge.join,
                         nc.df,
                         by=c("lat","lon")) %>%
    as.data.table() %>% 
    melt(., id.vars = c("lon","lat","weight","GEOID10","tribe"),
         variable.name = "date",
         value.name = "value") %>% 
    .[, keyby = .(GEOID10,tribe,date),
      .(weekPDSI = sum(value*weight/sum(weight, na.rm = T), na.rm = T))] %>% 
    .[, keyby = .(GEOID10,tribe),
      .("1980-2020_PDSI_Mean" = mean(weekPDSI))]
  
  write_rds(var.block,paste0("01_data/clean/b_drought_blocks/b_drought_",unique(fs$set),".rds"))
  print(unique(fs$set))
  
})

fl_all <- list.files("01_data/clean/b_drought_blocks", full.names = T)
f <- fl_all[1]

raw <- map_dfr(fl_all, function(f){
  
  temp <- read_rds(f)

})

write_rds(raw,"01_data/clean/b_drought_blocks.rds")




