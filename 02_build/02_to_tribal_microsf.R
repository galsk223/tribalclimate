library(tidyverse)
library(data.table)
library(janitor)
library(sf)
library(mapview)

rm(list = ls())
tribe_df <- read_rds("01_data/cache/tribe_bg_file_names.rds") %>%
  tidyr::separate(file_path,into = c("d1","d2","rt","tribe","file"),sep = "/") %>% 
  filter(rt %in% c("air_federal","air_state","otsa","sdtsa","tdsa")) %>% 
  dplyr::select(tribe,rt,state,county,tract,block)

tribe_act <- tribe_df %>% 
  mutate(GEOID_county = paste0(state,county)) 

todo <- setdiff(unique(tribe_act$GEOID_county),
                str_sub(list.files("01_data/cache/blocks_sf"),1,-5))

tribe_do <- tribe_act %>% 
  filter(GEOID_county %in% todo) %>% 
  group_split(GEOID_county)
ta <- tribe_do[[1]]
gc()
map(tribe_do,function(ta){
  
  blocks <- tigris::blocks(state = unique(ta$state),
                           county = unique(ta$county),
                           class = "sf")
  
  temp <- ta %>% 
    mutate(GEOID10 = paste0(GEOID_county,
                            str_pad(str_remove(tract,"[:punct:]"),
                                    width = 6,
                                    pad = "0"),
                            block)) %>% 
    right_join(blocks %>% 
                 dplyr::select(geometry, GEOID10), . , 
              by = c("GEOID10"))
  
  write_rds(temp, paste0("01_data/cache/blocks_sf/",
                         unique(temp$GEOID_county),".rds"))
  
  print(unique(temp$GEOID_county))
  
})


files <- list.files("01_data/cache/blocks_sf", full.names = T)
fl <- files[10]
all <- map_dfr(files,function(fl){
  
  r <- read_rds(fl)
  
})

# t2 <- read_csv("01_data/tribe_bg_full_integrated_combined_FINAL.csv") %>% 
#   filter(time == "time 2") %>% 
#   dplyr::select(tribe,UID = census_tribe, rt) %>% 
#   mutate(UID = str_extract(UID,"\\d+")) %>% 
#   unique() %>% 
#   filter(!is.na(UID),
#          !rt %in% c("tribalsub"))
# 
# all_save <- all %>% 
#   mutate(UID = str_extract(tribe,"\\d+")) %>% 
#   inner_join(.,t2,by="UID") 

class(all)

write_rds(all,"01_data/cache/tribe_shapefiles_micro.rds")
write_rds(all,"/RSTOR/cache/tribe_shapefiles_micro.rds")





