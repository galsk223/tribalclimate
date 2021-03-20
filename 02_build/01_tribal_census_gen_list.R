#This script constructs the full list of geoids for the tribes with folders

library(pacman)
p_load(tidyverse,lubridate,janitor,tigris,data.table)

#Set dir of google drive folder
setwd("S:/_LinuxMount/tribal_climate")

########################################
#Get all files

tribe_paths <- dir("01_data/tribal_bg_list",pattern = ".txt",recursive = T,full.names = T)

#x=tribe_paths[1]
#Loop over all files, read in data, and append path for rejoining to list
tribe_bg <- tribe_paths %>%
  map(function(x){
    tbg_list <- read_delim(x,delim = ";",col_types = "cccccccc") %>%
      clean_names()
    
    tbg <- tbg_list %>%
      mutate(geoid10=str_c(state,
                           county,
                           str_remove(tract,"\\."),
                           block)) %>%
      add_column(file_path=x)
    
    return(tbg)
  })

tribe_df <- rbindlist(tribe_bg,fill = T) ##Note: some tribal tract/bg data are missing, but I don't think we need it
#rnoaa::vis_miss(tribe_df)
saveRDS(tribe_df,"cache/tribe_bg_file_names.rds")

tribe_df_out <- tribe_df %>%
  separate(file_path,into = c("d1","d2","rt","tribe","file"),sep = "/") %>%
  select(geoid10,rt,tribe) %>%
  mutate(fips=str_sub(geoid10,1,5)) %>%
  inner_join(fips_codes %>% 
               mutate(fips=str_c(state_code,county_code),
                      county=str_remove(county," County")) %>% 
               select(fips,state,county))


write_csv(tribe_df_out,"cache/tribe_bg_full.csv")
