
setwd("/home/sambk223/tribalclimate")

library(raster)
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
library(furrr)

conflict_prefer("filter", "dplyr")
conflict_prefer("between", "dplyr")
conflict_prefer("last", "dplyr")
conflict_prefer("lag", "dplyr")
conflict_prefer("select", "dplyr")
conflict_prefer("year", "lubridate")
conflict_prefer("ggsave", "ggplot2")

Sys.setenv(RSTUDIO_PANDOC="/usr/lib/rstudio-server/bin/pandoc")

rm(list = ls())

# tribe_county <- tribedf[[1]]
  
if (length(list.files("01_data/cache/d_whp_blocks"))<495){
  
  message("Starting Extract Caching")
  
  #Access data in Arc grid file
  whp <- raster("01_data/RDS-2015-0046-2/Data/whp_2018_classified/whp2018_cls/w001001.adf")
  
  slicecoords <- tibble(lon = xyFromCell(whp, 1:ncell(whp))[,1],
                        lat = xyFromCell(whp, 1:ncell(whp))[,2]) %>%
    rownames_to_column(var = "cell") %>% 
    mutate(cell = as.numeric(cell))
  
  tribedf <- read_rds("01_data/cache/tribe_shapefiles_micro.rds") %>% 
    st_transform(crs(whp)) %>% 
    filter(!state %in% c("02","15")) 
  
  extract_done <- str_extract(list.files("01_data/cache/d_whp_blocks"),"\\d+")
  
  tribe_extract <- tribedf %>% 
    filter(!GEOID_county %in% extract_done) %>% 
    group_split(GEOID_county)
  
  plan(multisession)
  future_map(tribe_extract,function(tribe_county){
    
    p_crop <- raster::crop(whp,st_bbox(tribe_county))
    extracted <- terra::extract(p_crop,tribe_county,
                                cellnumbers = T)
    
    save <- list(extracted,tribe_county$GEOID10)
    
    write_rds(save,paste0("01_data/cache/d_whp_blocks/",unique(tribe_county$GEOID_county),".rds"))
    print(paste0("County ",unique(tribe_county$GEOID_county)," processed and cached (",nrow(tribe_county)," blocks)"))
    
  })
  plan(sequential)
  gc()
  message("Extract Caching Complete")
  
}



fl_done <- list.files("01_data/clean/d_whp_blocks")

fl <- tibble(fl_cached = list.files("01_data/cache/d_whp_blocks", full.names = T),
             county = str_sub(fl_cached,-9,-1)) %>% 
  filter(!county %in% fl_done)

f <- fl$fl_cached[1]
# e <- e1[1]
# GEOID10 <- geodf[1]

message("Starting Data Creation")
plan(multisession, workers = 4)
future_map(fl$fl_cached,function(f){
  
  whp <- tibble(GEOID10 = read_rds(f)[[2]],
                WHP = read_rds(f)[[1]]) %>% 
    unnest(WHP) %>% 
    mutate(WHP2 = ifelse(WHP[,2]>=6,0,WHP[,2])) %>% 
    group_by(GEOID10) %>% 
    summarise(WHP = mean(WHP2, na.rm = T)) %>% 
    ungroup()
  
  # geodf <- read_rds(f)[[2]]
  write_rds(whp,paste0("01_data/clean/d_whp_blocks/",unique(str_sub(whp$GEOID10,1,5)),".rds"))
  print(paste0("County ",unique(str_sub(whp$GEOID10,1,5))," data saved (",length(whp$GEOID10)," blocks)"))

})
plan(sequential)
gc()

fl_done <- list.files("01_data/clean/d_whp_blocks", full.names = T)

all <- map_dfr(fl_done,function(f){
  
  r <- read_rds(f)
  
})

write_rds(all,"01_data/clean/d_whp_blocks.rds")
