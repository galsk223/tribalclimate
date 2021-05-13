
rm(list = ls())

tribedf <- read_rds("01_data/cache/tribe_shapefiles.rds") 
tribeunqiue <- tribedf %>% 
  st_set_geometry(NULL) %>% 
  distinct(UID)

heat <- read_rds("01_data/clean/a_heat_gridmet.rds")
drought <- read_rds("01_data/clean/b_drought.rds") %>% 
  rename(drought_mean = `1980-2020 Mean`)
precip <- read_rds("01_data/clean/c_precip.rds")
whp <- read_rds("01_data/clean/d_whp.rds")
elrug <- read_rds("01_data/clean/e_ElevationAndRuggedness.rds")

wells_oil <- read_rds("01_data/clean/f_Oil_wells.rds") %>% 
  dplyr::select(UID,AllArea_OilPortion) %>% 
  mutate(AllArea_OilPortion = as.numeric(AllArea_OilPortion)) %>% 
  left_join(tribeunqiue,.,by="UID") %>% 
  replace(is.na(.), 0)

wells_gas <- read_rds("01_data/clean/f_Gas_wells.rds") %>% 
  dplyr::select(UID,AllArea_GasPortion) %>% 
  mutate(AllArea_GasPortion = as.numeric(AllArea_GasPortion)) %>% 
  left_join(tribeunqiue,.,by="UID") %>% 
  replace(is.na(.), 0)
  
OGbasins <- read_rds("01_data/clean/g_OilGas_basins.rds")
PAD <- read_rds("01_data/clean/h_federalland.rds") %>% 
  mutate(PADPortion = as.numeric(PADPortion))

soc <- map_dfr(list.files("01_data/cache/soc2", full.names = T),
               function(fl){
                 
                 t <- read_rds(fl)
                 
               }) %>% 
  group_by(UID) %>% 
  summarise(SOC_perc = mean(Interpolated_15_2)/10) %>% 
  ungroup()

all <- left_join(heat,drought,by="UID") %>% 
  left_join(.,precip,by="UID") %>% 
  left_join(.,whp,by="UID") %>% 
  left_join(.,elrug,by="UID") %>% 
  left_join(.,wells_oil,by="UID") %>% 
  left_join(.,wells_gas,by="UID") %>% 
  left_join(.,OGbasins,by="UID") %>% 
  left_join(.,PAD,by="UID") %>% 
  left_join(.,soc,by="UID") 

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







                