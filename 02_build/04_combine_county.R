
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
  filter(!GEOID %in% empty$GEOID)



heat <- read_rds("01_data/clean/a_heat_county.rds")
drought <- read_rds("01_data/clean/b_drought_county.rds")
precip <- read_rds("01_data/clean/c_precip_county.rds")
whp <- read_rds("01_data/clean/d_whp_county.rds")
elrug <- read_rds("01_data/clean/e_ElevationAndRuggedness_County.rds")

OGwells <- read_rds("01_data/clean/f_OilGas_Wells_County.rds") %>% 
  group_by(GEOID,Decade) %>% 
  summarise(AllOGAreaPortion = mean(as.numeric(AllOGAreaPortion))) %>% 
  ungroup() %>% 
  pivot_wider(names_from = Decade,
              names_prefix = "OGAreaPortion_",
              values_from = AllOGAreaPortion) %>% 
  left_join(tribeuse,.,by="GEOID") %>% 
  st_set_geometry(NULL) %>% 
  replace(is.na(.), 0)
  
OGbasins <- read_rds("01_data/clean/g_OilGas_basins_county.rds") %>% 
  st_set_geometry(NULL) 
PAD <- read_rds("01_data/clean/h_federalland_county.rds")


all <- tribedf %>% 
  st_set_geometry(NULL) %>% 
  left_join(.,heat,by="GEOID") %>% 
  left_join(.,drought,by="GEOID") %>% 
  left_join(.,precip,by="GEOID") %>% 
  left_join(.,whp,by="GEOID") %>% 
  left_join(.,elrug,by="GEOID") %>% 
  left_join(.,OGwells,by="GEOID") %>% 
  left_join(.,OGbasins,by="GEOID") %>% 
  left_join(.,PAD,by="GEOID") 

sums <- all %>% 
  dplyr::select(-contains(c("q25","q75","sd","min","median","max","GEOID","tribe"))) %>% 
  summarise_all(list(
                   N = ~sum(!is.na(.)),
                   Min = ~min(., na.rm = T),
                   Mean = ~mean(., na.rm = T),
                   Max = ~max(., na.rm = T))) %>% 
  pivot_longer(everything()) %>% 
  mutate(Stat = str_remove(str_extract(name,"_N|_Min|_Mean|_Max"),"_"),
         Variable = str_remove(str_remove(name,"_N|_Min|_Mean|_Max"),"_"),
         value = round(value,3)) %>% 
  pivot_wider(-name,
              names_from = Stat)

write_csv(all,"01_data/clean/00_all_countypolygons.csv")







                