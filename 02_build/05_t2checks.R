library(sf)

rm(list = ls())


t2 <- read_csv("01_data/tribe_bg_full_integrated_combined_FINAL.csv") %>% 
  filter(time == "time 2") %>% 
  select(tribe,UID = census_tribe, rt) %>% 
  mutate(UID = str_extract(UID,"\\d+")) %>% 
  unique() %>% 
  filter(!is.na(UID))

tribes_1 <- read_rds("01_data/cache/tribe_shapefiles.rds") %>% 
  st_set_geometry(NULL) %>% 
  inner_join(.,t2,by=c("AIANNHCE" = "UID")) %>% 
  select(tribe, everything()) 

tribes_2 <- read_rds("01_data/cache/tribe_shapefiles.rds") %>% 
  st_set_geometry(NULL) %>% 
  inner_join(.,t2,by=c("UID" = "UID")) %>% 
  select(tribe, everything()) 

n_distinct(tribes_2$tribe) == n_distinct(tribes_1$tribe)

%>% 
  mutate(Flag = ifelse(UID == GEOID,1,0)) %>% 
  group_by(tribe) %>% 
  summarise(n = n(),
            Flag = sum(Flag)) %>% 
  ungroup() %>% 
  filter(Flag > 0) 

tribetest <- read_rds("01_data/cache/tribe_shapefiles.rds") %>% 
  inner_join(.,t2,by=c("AIANNHCE" = "UID")) %>% 
  filter(tribe %in% tribes$tribe) %>% 
  mutate(Flag = ifelse(UID == GEOID,1,0))

tribe_big <- tribetest %>% 
  filter(Flag == 0) %>% 
  group_split(tribe)

tribe_small <- tribetest %>% 
  filter(Flag == 1) %>% 
  group_split(tribe)

n <- 26
mapview::mapview(tribe_big[[n]]) + mapview::mapview(tribe_small[[n]])
glimpse(tribe_big[[n]])
glimpse(tribe_small[[n]])

# not same
#  big covers more, Cherokee, Cheyenne, Chippewa, Choctaw

view(tribetest[[n]] %>% st_set_geometry(NULL))
