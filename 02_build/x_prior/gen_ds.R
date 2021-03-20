#Script to generate variables for NA_relocation



# libraries used 
pacs <- c("tidyverse","lubridate","progress","foreach","doSNOW","conflicted",
          "USAboundaries","raster","rgdal","sf","gridExtra","ncdf4","foreign","viridis")
lapply(pacs, require, character.only = TRUE)

setwd("E:/Data/NA_relocation")

conflict_prefer("select", "dplyr")
conflict_prefer("filter", "dplyr")
######################################
#Load drought data
load("pdsi_weekly_county_1979-2017.Rdata")

#Load wildfire hazard potential data
load("county_whp.Rdata")

#######################################
#Calculate county average across all data
reloc.all <- gridmet.county %>%
  group_by(county) %>%
  summarize(mean_pdsi=mean(value,na.rm=T),
            median_pdsi=quantile(value,p=.5,na.rm=T)) %>%
  ungroup()

#Calculate decadal changes
reloc.decade <- gridmet.county %>%
  mutate(value=value+10,
         decade=floor((year(date)-1900)/10)) %>%
  group_by(county,decade) %>%
  summarize(mean_pdsi=mean(value,na.rm=T),
            median_pdsi=quantile(value,p=.5,na.rm=T)) %>%
  ungroup() %>%
  filter(decade %in% c(8,11)) %>%
  group_by(county) %>%
  mutate_at(vars(mean_pdsi,median_pdsi),~(.-dplyr::lag(.))) %>%
  ungroup()


#################################
#Export data
reloc <- left_join(county.whp,
                    reloc.decade %>%
                      filter(decade==11) %>%
                      select(-decade) %>%
                      rename(geoid=county,median_pdsi_10_80=median_pdsi,mean_pdsi_10_80=mean_pdsi),
                    by="geoid") %>% 
  left_join(.,
            reloc.all,
            by=c("geoid"="county")) %>%
  rename(fips=geoid)

write_csv(reloc, path = "county_whp_pdsi.csv")

#################################
#Import prepped conus counties
load("geographies/geos.Rdata")

#whp in space
county.whp %>%
  gather(-geoid,key = "var",value = "value") %>%
  inner_join(us_co %>% select(geoid),.) %>%
  ggplot() +
  geom_sf(aes(fill=value),color=NA) +
  geom_sf(data = us_st,color="white",fill=NA) +
  scale_fill_viridis() +
  facet_wrap(~var)



reloc.decade %>%
  filter(decade==11) %>%
  select(-decade) %>%
  rename(geoid=county) %>%
  gather(-geoid,key = "var",value = "value") %>%
  inner_join(us_co %>% select(geoid) %>% st_transform(5070),.) %>%
  ggplot() +
  geom_sf(aes(fill=value),color=NA) +
  geom_sf(data = us_st,fill=NA) +
  scale_fill_gradient2(low = "red",high = "green") +
  facet_wrap(~var) + 
  theme_bw()


reloc.all %>%
  rename(geoid=county) %>%
  inner_join(us_co %>% select(geoid) %>% st_transform(5070),.) %>%
  ggplot() +
  geom_sf(aes(fill=mean_pdsi),color=NA) +
  geom_sf(data = us_st,fill=NA) +
  scale_fill_gradient2(low = "red",high = "green") +
  theme_bw()
