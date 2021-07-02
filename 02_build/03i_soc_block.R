
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

# from https://scholarsphere.psu.edu/resources/ea4b6c45-9eba-4b89-aba6-ff7246880fb1
#  described https://github.com/aramcharan/US_SoilGrids100m

s000 <- raster("01_data/sl1.tif") 
s005 <- raster("01_data/sl2.tif") 
s015 <- raster("01_data/sl3.tif") 
s030 <- raster("01_data/sl4.tif") 
sall <- stack(c(s000,s005,s015,s030))

fldone <- list.files("01_data/cache/i_soc_block")
geoiddone <- str_extract(fldone,"\\d+")

todo <- setdiff(unique(tribedf$GEOID10),
                geoiddone) %>%
  as.numeric()

t <- tribedf %>% 
  filter(GEOID10 %in% todo)

# ts <- setdiff(todo,t$GEOID10)

geoid <- t$GEOID10[[10]]

map(t$GEOID10,function(geoid){
  
  tribeuse <- tribedf %>% 
    filter(GEOID10 == geoid)
  
  extracted <- terra::extract(sall,tribeuse)
  
  if(length(as.data.table(extracted[1]))>0){
    if(nrow(filter(as.data.table(extracted[1]),!is.na(sl1)))>0){ 
    
    e1dt_long <- extracted[1] %>% 
      as.data.table() %>% 
      filter(!is.na(sl1)) %>% 
      dplyr::select(sl2,sl3,sl4) %>% 
      pivot_longer(everything()) %>% 
      mutate(name = case_when(name == "sl2" ~ 5,
                              name == "sl3" ~ 15,
                              name == "sl4" ~ 30))
    
    r <- lm(value ~ name, e1dt_long)
    
    save <- tibble(GEOID10 = geoid,
                   Interpolated_15 = r$coefficients[1]+r$coefficients[2]*15) 
    
    write_rds(save,paste0("01_data/cache/i_soc_block/",geoid,".rds"))
    
    print(paste0(geoid," - ",Sys.time()))
    
  }}

})

flsoc <- list.files("01_data/cache/i_soc_block", full.names = T)
f <- flsoc[[1]]
soc <- map_dfr(flsoc,function(f){
  
  temp <- read_rds(f)
  print(match(f,flsoc))
  return(temp)
  
})
write_rds(soc,"01_data/clean/i_soc_blocks.rds")  



