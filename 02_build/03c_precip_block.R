
# from https://prism.oregonstate.edu

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

tribedf <- read_rds("01_data/cache/tribe_shapefiles_micro.rds") %>% 
  filter(!state %in% c("02","15")) %>% 
  group_split(GEOID_county)

prism <- raster("01_data/PRISM_ppt_30yr_normal_4kmM2_annual_asc/PRISM_ppt_30yr_normal_4kmM2_annual_asc.asc")

slicecoords <- tibble(lon = xyFromCell(prism, 1:ncell(prism))[,1],
                      lat = xyFromCell(prism, 1:ncell(prism))[,2]) %>%
  rownames_to_column(var = "cell") %>% 
  mutate(cell = as.numeric(cell))

# tribe_county <- tribedf[[14]]

if (length(list.files("01_data/cache/c_precip_blocks"))<495){
  
  plan(multisession)
  future_map(tribedf,function(tribe_county){
    
    p_crop <- terra::crop(prism,st_bbox(tribe_county))
    extracted <- terra::extract(p_crop,tribe_county,
                                cellnumbers = T)
    
    save <- list(extracted,tribe_county$GEOID10)
    
    write_rds(save,paste0("01_data/cache/c_precip_blocks/",unique(tribe_county$GEOID_county),".rds"))
    print(paste0("County ",unique(tribe_county$GEOID_county)," processed and cached (",nrow(tribe_county)," blocks)"))
    
  })
  plan(sequential)
  gc()
  
}


fl <- list.files("01_data/cache/c_precip_blocks", full.names = T)
# f <- fl[[1]]
# e <- e1[2]
# GEOID10 <- tribe_county$GEOID10[1]

map(fl,function(f){
  
    e1 <- read_rds(f)[[1]]
    geodf <- read_rds(f)[[2]]
    
    precip <- map2_dfr(e1,geodf,function(e,GEOID10){
      
      e1dt <- e %>% 
        as.data.table()
      
      if (nrow(e1dt) > 0){
        
        temp <- e1dt %>% 
          merge(., slicecoords, all.x=T, by="cell") %>%
          .[, c("precip","GEOID10") :=
              .(value,GEOID10)] %>% 
          group_by(GEOID10) %>% 
          summarise(precip = mean(precip, na.rm = T)) %>% 
          ungroup()
        
      }
      
    })
    
    write_rds(precip,paste0("01_data/clean/c_precip_blocks/",unique(str_sub(geodf,1,5)),".rds"))
    print(paste0("County ",unique(str_sub(geodf,1,5))," data saved (",length(geodf)," blocks)"))
    
})
  
fl_all <- list.files("01_data/clean/c_precip_blocks", full.names = T)
f <- fl_all[1]

raw <- map_dfr(fl_all, function(f){
  
  temp <- read_rds(f) 
  
})

write_rds(raw,"01_data/clean/c_precip_blocks.rds")


