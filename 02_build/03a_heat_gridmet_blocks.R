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
library(future)

conflict_prefer("filter", "dplyr")
conflict_prefer("between", "dplyr")
conflict_prefer("last", "dplyr")
conflict_prefer("lag", "dplyr")
conflict_prefer("year", "lubridate")
conflict_prefer("ggsave", "ggplot2")

Sys.setenv(RSTUDIO_PANDOC="/usr/lib/rstudio-server/bin/pandoc")

rm(list = ls())

# from https://www.arcgis.com/home/item.html?id=680e87c5b1d34e0585203aa4f67d8426

# tribedf <- read_rds("01_data/cache/tribe_shapefiles_micro.rds")

file.list <- expand.grid("tmmx",1979:2001,stringsAsFactors = F) %>% 
  rename(var=Var1,year=Var2) %>%
  mutate(var=str_to_lower(var),
         file.name = str_c(var,"_",year,".nc")) %>% 
  arrange(var) 

allheat <- paste0("01_data/tmmx/",file.list$file.name)
heat1 <- allheat[length(allheat)]

fl <- tibble(fl = list.files("01_data/cache/bridge_cache", full.names = T)) %>% 
  add_rownames() %>% 
  mutate(set = floor(as.numeric(rowname)/1000)) %>% 
  group_split(set)

todo <- expand_grid(set = 0:408,
                    yrs = 1979:2001)

done <- tibble(fl = list.files("01_data/clean/a_heat_blocks/"),
               set = str_sub(fl,8,-10) %>%  
                 as.numeric(),
               yrs = str_sub(fl,-8,-5) %>%  
                 as.numeric()) %>% 
  group_by(set) %>% 
  tally()

fs <- fl[[409]]

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
  
  plan(multisession)
  future_map(fl,function(fs){
    
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
        .(heatdays = sum(value > 310.928, na.rm = T))] %>% 
      .[, keyby = .(GEOID10,tribe),
        .(heatdays_year = mean(heatdays, na.rm = T))]
    
    write_rds(var.block,paste0("01_data/clean/a_heat_blocks/a_heat_",unique(fs$set),"-",str_sub(heat1,-7,-4),".rds"))
    print(unique(fs$set))
    
  })
  plan(sequential)
  
})

fl_all <- list.files("01_data/clean/a_heat_blocks", full.names = T)
f <- fl_all[1]

raw <- map_dfr(fl_all, function(f){
  
  temp <- read_rds(f) %>% 
    mutate(Year = str_sub(f,-8,-5) %>% 
             as.numeric())
  
})

tosave <- raw %>% 
  group_by(tribe,GEOID10) %>% 
  summarise(Mean_HeatDays = mean(heatdays_year)) %>% 
  ungroup()

write_rds(tosave,"01_data/clean/a_heat_blocks.rds")
