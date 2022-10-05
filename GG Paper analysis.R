## GG Paper
# Federico Tomasetto 
# 2022/10/4

#rm(list = ls())

library(tidyverse)

setwd("C:/Users/TOMASETTOF/OneDrive - AgResearch/ggj_paper/R analysis/GG Paper")

gg1 <- read.csv('C:/Users/TOMASETTOF/OneDrive - AgResearch/ggj_paper/materials/rs_gg_mdl_data_2022_01_07.csv')      
glimpse(gg1)

library(GGally)

# Create data 
gg1.data <- data.frame(gg1$Mean.m.2, gg1[,16:25]) 
x11()
ggpairs(gg1.data, title="Correlogram between Vegetation Indexes from Planet Lab vs. grass grub densities")

#p_title <- 'Vegetation Indexes from Planet Lab' 

#p1 <- ggplot() +
#      theme_bw() +
#      geom_point(data = gg1, aes(x = Blue, y = Mean.m.2), pch = 21, size = 3, alpha = .7, colour = 'grey50', fill = 'chartreuse4') +
#      geom_smooth(data = gg1, aes(x = Blue, y = Mean.m.2), method = 'lm') +
#      labs(title = p_title)+
#      facet_wrap(~year, ncol = 2)
