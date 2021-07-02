library(tidyverse)
library(data.table)
library(janitor)
library(sf)
library(mapview)
library(raster)
library(terra)
library(lubridate)
library(downloader)
library(ncdf4)
library(elevatr)
library(furrr)



# -------------------------------------------
# T1 - tribe census polygons 
# -------------------------------------------

source("02_build/03a_heat_gridmet.R.R")
source("02_build/03b_drought.R")
source("02_build/03c_precip.R")
source("02_build/03d_whp.R")

source("02_build/03e_elevation_ruggedness.R")
source("02_build/03f_oilgas_wells.R")
source("02_build/03g_oilgas_basins.R")
source("02_build/03h_federallands.R")
source("02_build/03i_soc.R")

source("02_build/04_combine.R")


# -------------------------------------------
# T1 block polygons
# -------------------------------------------

source("02_build/03a_heat_gridmet_block.R.R")
source("02_build/03b_drought_block.R")
source("02_build/03c_precip_block.R")
source("02_build/03d_whp_block.R")

source("02_build/03e_elevation_ruggedness_block.R")
source("02_build/03f_oilgas_wells_block.R")
source("02_build/03g_oilgas_basins_block.R")
source("02_build/03h_federallands_block.R")
source("02_build/03i_soc_block.R")

source("02_build/04_combine_block.R")



# -------------------------------------------
# T2 - tribe county polygons
# -------------------------------------------

source("02_build/03a_heat_gridmet_county.R")
source("02_build/03b_drought_county.R") # run again
source("02_build/03c_precip_county.R")
source("02_build/03d_whp_county.R")

source("02_build/03e_elevation_ruggedness_county.R")
source("02_build/03f_oilgas_wells_county.R")
source("02_build/03g_oilgas_basins_county.R")
source("02_build/03h_federallands_county.R")
source("02_build/03i_soc_county.R")

source("02_build/04_combine_county.R")


