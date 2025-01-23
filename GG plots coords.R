# Create lat and long for GG plots

# Federico Tomasetto 
# 2025/1/23

rm(list = ls(all.names = TRUE)) # will clear all objects, including hidden objects
gc()

library(tidyverse)
library(xlsx)

setwd("C:/Users/TOMASETTOF/OneDrive - AgResearch/Documents/GitHub/GGPaper")

gg1 <- read.csv('C:/Users/TOMASETTOF/OneDrive - AgResearch/Documents/GitHub/GGPaper/rs_gg_mdl_data_2022_01_07_plus_2012_2013_FT.csv')      
glimpse(gg1)

gg1.coord <-  read.xlsx('C:/Users/TOMASETTOF/OneDrive - AgResearch/Documents/GitHub/GGPaper/GG_plots_coord.xls', sheetIndex=1, header=TRUE, colClasses=NA)      
glimpse(gg1.coord)

gg1.comp <- full_join(gg1, gg1.coord, by = "PlotID")

write.csv(gg1.comp,"C:/Users/TOMASETTOF/OneDrive - AgResearch/Documents/GitHub/GGPaper/gg.fulldata.csv", row.names = FALSE)
