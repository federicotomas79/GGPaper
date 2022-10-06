## GG Paper
# Federico Tomasetto 
# 2022/10/4

#rm(list = ls())

library(tidyverse)

setwd("C:/Users/TOMASETTOF/OneDrive - AgResearch/ggj_paper/R analysis/GG Paper")

gg1 <- read.csv('C:/Users/TOMASETTOF/OneDrive - AgResearch/ggj_paper/materials/rs_gg_mdl_data_2022_01_07.csv')      
glimpse(gg1)

#Calculate and add difference in sampling days
gg.sample.days <- as.Date(as.character(gg1$gg_sample_Date), format="%d/%m/%Y")
unique(gg.sample.days)
rs.sample.days <- as.Date(as.character(gg1$rs_sample_Date), format="%d/%m/%Y")
unique(rs.sample.days)

diff.days.sample <- difftime(rs.sample.days, gg.sample.days, units = "days")
unique(diff.days.sample)
gg1$diff.days.sample <- as.numeric(diff.days.sample)
glimpse(gg1)

#visualize all variables in the dataset
library(reshape2)

melt.gg1 <- melt(gg1[, c(9,16:25,29)])
melt.gg1$year <- gg1$year

ggplot(data = melt.gg1, aes(x = value, group=year, colour=year)) +
       stat_density(geom = "path", position = "identity") + 
       theme_bw() +
       scale_color_gradientn(colours = rainbow(5))+
       facet_wrap(~variable, scales = "free") 

#Create correlations of all of the numerical variables
library(GGally)

gg1.data <- data.frame(gg1$Mean.m.2, gg1[,16:25]) 
x11()
ggpairs(gg1.data, title="Correlogram - Vegetation Indexes from Planet Lab vs. grass grub densities")

#p_title <- 'Vegetation Indeces from Planet Lab' 

#p1 <- ggplot() +
#      theme_bw() +
#      geom_point(data = gg1, aes(x = Blue, y = Mean.m.2), pch = 21, size = 3, alpha = .7, colour = 'grey50', fill = 'chartreuse4') +
#      geom_smooth(data = gg1, aes(x = Blue, y = Mean.m.2), method = 'lm') +
#      labs(title = p_title)+
#      facet_wrap(~year, ncol = 2)
