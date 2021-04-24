
rm(list = ls())
tribedf <- read_rds("01_data/cache/tribe_shapefiles.rds") 

# from https://scholarsphere.psu.edu/resources/ea4b6c45-9eba-4b89-aba6-ff7246880fb1
#  described https://github.com/aramcharan/US_SoilGrids100m

s000 <- raster("01_data/sl1.tif") 
s005 <- raster("01_data/sl2.tif") 
s015 <- raster("01_data/sl3.tif") 
s030 <- raster("01_data/sl4.tif") 
sall <- stack(c(s000,s005,s015,s030))

todo <- setdiff(1:nrow(tribedf),
                str_extract(list.files("01_data/cache/soc"),"\\d+")) %>% 
  as.numeric()
  
n <- todo[1]

# 20 hours
soc <- map(todo,function(n){
  
  extracted <- terra::extract(sall,tribedf[n,],
                              weights = T, 
                              normalizeWeights = T,
                              cellnumbers = T)
  
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
    
    e1dt <- extracted[1] %>% 
      as.data.table() %>% 
      mutate(valueaggregated2 = 
               sl2*1/3+
               sl3*1/3+
               sl4*1/3,
             valueweighted2 = valueaggregated2*weight,) %>% 
      summarise(Equal_5_15_30 = sum(valueweighted2, na.rm = T)) %>% 
      mutate(UID = tribedf$UID[n],
             Interpolated_15 = r$coefficients[1]-r$coefficients[2]*15) %>% 
      dplyr::select(UID,everything())
    
    write_rds(e1dt,paste0("01_data/cache/soc/",n,".rds"))
    
    # print(n)
    return(e1dt)
    
    }}
  
})

# write_rds(soc,"01_data/clean/i_soc.rds")



# socplot <- soc %>% 
#   pivot_longer(-UID)
# 
# p1 <- ggplot(socplot, aes(x = name, y = value)) +
#   geom_point(aes(color = UID, shape = UID), size = 4) +
#   ggthemes::theme_tufte(base_size = 12) +
#   labs(x = "Method",
#        y = "SOC Aggregated Value",
#        title = "Ordinal Relationship of Tribe Aggregated values by Method")
# 
# p1
# 
# p2 <- ggplot(socplot, aes(x = UID, y = value)) +
#   geom_point(aes(color = name, shape = name), size = 4) +
#   ggthemes::theme_tufte(base_size = 12) +
#   labs(x = "Method",
#        y = "SOC Aggregated Value",
#        title = "Ordinal Relationship of Method Aggregated values by Tribe")
# 
# p2









