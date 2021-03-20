#I downloaded all of the data in the tribal folders on the census ftp site.
#These data contained many pdf files that I don't need.  This script extracts
#only the text files and removes the pdf files in the folders.

library(pacman)
p_load(tidyverse,lubridate,janitor,tigris)

#Set dir of google drive folder
setwd("L:/My Drive/Projects/tribal/tribal_bg_list")

########################################
#Get all files

tribe_paths <- dir("",pattern = ".txt",recursive = T)

all_tribes <- tribe_paths %>%
  enframe(name = NULL,value = "path") %>%
  separate(path,into = c("rt","tribe","file"),sep = "/")

write_csv(all_tribes,"../all_tribe_files.csv")

dir(pattern = ".txt",recursive = T)[1] %>%
  map(function(x){
    
    file.copy(from = str_c("E:/temp/tribal_bg_list",x),
              to = str_c("L:/My Drive/Projects/tribal/tribal_bg_list/",x),
              recursive = T)
  })

test <- all_tribes %>%
  group_split(split_var=row_number()) 

x=test[[1]]
map(function(x){
  dir.create()
  file.copy(from = str_c("E:/temp/tribal_bg_list",x),
            to = str_c("L:/My Drive/Projects/tribal/tribal_bg_list/",x),
            recursive = T)
})

#remove all pdf files
dir(pattern = ".pdf",recursive = T) %>%
  map(function(x){
    
    file.remove(x)
  })


