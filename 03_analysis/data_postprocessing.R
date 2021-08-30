#This script reads in the dataset used for analysis and 
#renames variables for easier interpretation

# Tribes that appear in both periods
t1t2_ds <- read_rds("03_analysis/data_long.rds")

final_t1t2_ds <- t1t2_ds %>%
  select(time, tribe,
         heatdays=heatdays_mean,
         drought=drought_mean,
         precip,
         whp,
         oil_portion=AllArea_OilPortion,
         gas_portion=AllArea_GasPortion,
         og_basin_portion=BasinPortion,
         federal_lands_portion=PADPortion,
         soc=SOC_perc,
         elevation=elevation_mean,
         tri=tri_mean) 

write_csv(final_t1t2_ds,"/RSTOR/tribal_climate/data_products/tribal_dispossession_analysis_both_periods.csv")

# Tribes that appear in both periods
all_ds <- read_rds("03_analysis/data_long_all.rds")

final_all_ds <- all_ds %>%
  select(time, tribe,
         heatdays=heatdays_mean,
         drought=drought_mean,
         precip,
         whp,
         oil_portion=AllArea_OilPortion,
         gas_portion=AllArea_GasPortion,
         og_basin_portion=BasinPortion,
         federal_lands_portion=PADPortion,
         soc=SOC_perc,
         elevation=elevation_mean,
         tri=tri_mean) 

write_csv(final_all_ds,"/RSTOR/tribal_climate/data_products/tribal_dispossession_analysis_all.csv")
