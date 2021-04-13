
rm(list = ls())

tribedf <- read_rds("01_data/cache/tribe_shapefiles.rds") 
tribeunqiue <- tribedf %>% 
  st_set_geometry(NULL) %>% 
  distinct(UID)

heat <- read_rds("01_data/clean/a_heat.rds")
drought <- read_rds("01_data/clean/b_drought.rds")
precip <- read_rds("01_data/clean/c_precip.rds")
whp <- read_rds("01_data/clean/d_whp.rds")
elrug <- read_rds("01_data/clean/e_ElevationAndRuggedness.rds")

OGwells <- read_rds("01_data/clean/f_OilGas_wells.rds") %>% 
  dplyr::select(UID,Decade,AllOGAreaPortion) %>% 
  mutate(AllOGAreaPortion = as.numeric(AllOGAreaPortion)) %>% 
  pivot_wider(names_from = Decade,
              names_prefix = "OGAreaPortion_",
              values_from = AllOGAreaPortion) %>% 
  left_join(tribeunqiue,.,by="UID") %>% 
  replace(is.na(.), 0)
  
OGbasins <- read_rds("01_data/clean/g_OilGas_basins.rds")
PAD <- read_rds("01_data/clean/h_federalland.rds") %>% 
  mutate(PADPortion = as.numeric(PADPortion))

all <- left_join(heat,drought,by="UID") %>% 
  left_join(.,precip,by="UID") %>% 
  left_join(.,whp,by="UID") %>% 
  left_join(.,elrug,by="UID") %>% 
  left_join(.,OGwells,by="UID") %>% 
  left_join(.,OGbasins,by="UID") %>% 
  left_join(.,PAD,by="UID") 

sums <- all %>% 
  dplyr::select(-contains(c("q25","q75","sd","min","median","max","UID"))) %>% 
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

write_csv(all,"01_data/clean/00_all_newpolygons.csv")







                