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
# tribe census polygons
# -------------------------------------------

source("02_build/03a_heat.R")
source("02_build/03b_drought.R")
source("02_build/03c_precip.R")
source("02_build/03d_whp.R")

source("02_build/03e_elevation_ruggedness.R")
source("02_build/03f_oilgas_wells.R")
source("02_build/03g_oilgas_basins.R")
source("02_build/03h_federallands.R")

source("02_build/04_combine.R")



# -------------------------------------------
# tribe county polygons
# -------------------------------------------

source("02_build/03a_heat.R")
source("02_build/03a_heat_county.R")
source("02_build/03b_drought_county.R")
source("02_build/03c_precip.R")
source("02_build/03c_precip_county.R")
source("02_build/03d_whp_county.R")

source("02_build/03e_elevation_ruggedness_county.R")
source("02_build/03f_oilgas_wells_county.R")
source("02_build/03g_oilgas_basins_county.R")
source("02_build/03h_federallands_county.R")

source("02_build/04_combine_county.R")


