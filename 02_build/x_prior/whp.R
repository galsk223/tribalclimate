#This script reads the wildfire hazard potential 
#Dillon, Gregory K. 2018. Wildfire Hazard Potential (WHP) for the conterminous United States (270-m GRID), version 2018 classified. 2nd Edition. Fort Collins, CO: Forest Service Research Data Archive. https://doi.org/10.2737/RDS-2015-0046-2
#1) very low, 2) low, 3) moderate, 4) high, and 5) very high. 
#In addition, non-burnable lands (6) and open water (7) are represented as separate classes.

setwd("E:/Data/Wildfire/Wildfire_Hazard_Potential")

pacs <- c("tidyverse","lubridate","progress","foreach","doSNOW","conflicted",
          "USAboundaries","raster","rgdal","sf","gridExtra","ncdf4","foreign","viridis")
lapply(pacs, require, character.only = TRUE)

conflict_prefer("extract", "raster")
conflict_prefer("select", "dplyr")
##############################################################################
#Access data in Arc grid file
whp <- raster("RDS-2015-0046-2/Data/whp_2018_classified/whp2018_cls/w001001.adf")
#hist(whp)

#Import prepped conus counties
load("geographies/geos.Rdata")
us_co <- st_transform(us_co,crs = proj4string(whp))

#Extract the raster values that lie under each polygon
if(file.exists("whp_co_extract.Rdata")){
  load("whp_co_extract.Rdata")
} else {
  temp <- extract(whp,us_co)  
  save(temp,file = "whp_co_extract.Rdata")
}


#Calculating the mean WHP in and around each community
output <- map(temp,function(x){
  y <- ifelse(x>=6,0,x)
  out <- tibble(whp_mean=mean(y,na.rm=T),
                whp_q25=quantile(y,p=.25,na.rm=T),
                whp_median=quantile(y,p=.5,na.rm=T),
                whp_q75=quantile(y,p=.75,na.rm=T))
  return(out)
  }) 


#Appending the calculated output to the community dataframe *** note the assumption that raster::extract maintains order
county.whp <- us_co %>%
  select(geoid) %>%
  st_set_geometry(NULL) %>%   #Converting community shapes to dataframe for merging
  bind_cols(.,bind_rows(output))

save(county.whp,file = "county_whp.Rdata")  
write_csv(county.whp, path = "county_whp.csv")


