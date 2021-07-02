
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
library(elevatr)

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
  filter(!state %in% c("02","15")) 

#Begin loop over counties
done <- tibble(files = list.files("01_data/cache/e_elevrugg_blocks/")) %>% 
  mutate(GEOID_county = str_sub(files,1,-5))

tribedo <- tribedf %>% 
  filter(!GEOID_county %in% done$GEOID_county) %>% 
  group_split(GEOID_county)

# t <- unique(tribedo$GEOID_county)[1]

if (length(tribedo)>0){
  
  plan(multisession, workers = 4)
  future_map(tribedo,function(t){
    
    #Generate DEM for county
    elev.raster <- get_elev_raster(t,z=9,expand = 1)
    names(elev.raster) <- "elevation"
    
    #Calculating topographic information and appending to elevation
    topo.raster <- stack(elev.raster,terrain(elev.raster,opt = c("slope","aspect","TRI")))
    
    #Extract elevation data
    elev.temp <- terra::extract(topo.raster,
                                t)
    
    write_rds(elev.temp,paste0("01_data/cache/e_elevrugg_blocks/",unique(t$GEOID_county),".rds"))
    
  })
  plan(sequential)
  gc()
  
}


fl_cache <- list.files("01_data/cache/e_elevrugg_blocks", full.names = T)
# f <- fl_cache[10]
# et <- 1

map(fl_cache,function(f){
  
  elev.temp <- read_rds(f)
  t <- str_sub(f,-9,-5)
  
  geodf <- tribedf %>% 
    filter(GEOID_county == t)
  
  out <- map_dfr(1:length(elev.temp),function(et){
    
    r <- as_tibble(elev.temp[[et]]) %>% 
      summarize_all(funs(mean=mean(.,na.rm=T),
                         sd=sd(.,na.rm=T))) %>% 
      mutate(GEOID10 = geodf$GEOID10[et])

  }) 
  
  write_rds(out, paste0("01_data/clean/e_elevrugg_blocks/",t,".rds"))
  
}) 


fl_final <- list.files("01_data/clean/e_elevrugg_blocks/", full.names = T)

wire.save <- map_dfr(fl_final,function(f){
  
  r <- read_rds(f)
  
})

write_rds(wire.save,"01_data/clean/e_elevrugg_blocks.rds")




