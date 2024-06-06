## GG Paper
# Federico Tomasetto 
# 2024/6/06

rm(list = ls(all.names = TRUE)) # will clear all objects, including hidden objects
gc()

library(tidyverse)

setwd("C:/Users/TOMASETTOF/OneDrive - AgResearch/Documents/GitHub/GGPaper")

gg1 <- read.csv('C:/Users/TOMASETTOF/OneDrive - AgResearch/Documents/GitHub/GGPaper/rs_gg_mdl_data_2022_01_07_plus_2012_2013_FT.csv')      
glimpse(gg1)

table(gg1$year)

#Filter with Year > 2013
gg1.2014 <- filter(gg1, year >= 2014)
table(gg1.2014$year)

#MSAVI = modified soil-adjusted vegetation index (index less sensitive to chlorophyll effects, more responsive to green LAI variations and more resistant to soil and atmosphere effects)
#NGRDI = Normalized Green–Red Difference Index indicates the colour of a pixel (i.e., greenish or reddish)
#ReNDVI = Red-edge Normalized Vegetation Index approaches 1 when the crops are dense and the RENDVI approaches 0 when the crops are thin

#Calculate and add difference in sampling days
gg.sample.days <- as.Date(as.character(gg1.2014$gg_sample_Date), format="%d/%m/%Y")
unique(gg.sample.days)
rs.sample.days <- as.Date(as.character(gg1.2014$rs_sample_Date), format="%d/%m/%Y")
unique(rs.sample.days)

diff.days.sample <- difftime(rs.sample.days, gg.sample.days, units = "days")
unique(diff.days.sample)
gg1.2014$diff.days.sample <- as.numeric(diff.days.sample)
glimpse(gg1.2014)

write.csv(gg1.2014,file="RS_GG_data_2014_2017.csv", row.names=FALSE)

#Graphs for difference days between rs and gg sample
#x11()
ggplot(gg1.2014, aes(x = year, y = diff.days.sample, fill=diff.days.sample)) +
  geom_point(stat = 'identity', color="grey", show.legend = FALSE) +
  theme_bw() +
  labs(title = "Difference in days between gg sampling and rs data", y = "Difference in days (RS images)", x = "Sampling year")+
  stat_summary(fun.data=mean_sdl, fun.args = list(mult=1), geom="point", color="red", show.legend = FALSE) +
  scale_y_continuous(breaks = (c(0, -50, -100, -150, -200)), labels = c("Day GG sampled", -50, -100, -150, -200))

#How do I calculate the effect of each grey dot (satellite image) to the GG density (Mean.m.2)?

#Explore ACF for each year = 2014
gg1.2014v2 <- filter(gg1, year == 2014)
table(gg1.2014v2$year)
glimpse(gg1.2014v2)

acf(gg1.2014v2[gg1.2014v2$Ryegrass.cultivar=="Alto",]$reNDVI)
acf(gg1.2014v2[gg1.2014v2$Ryegrass.cultivar=="Halo",]$reNDVI)
acf(gg1.2014v2[gg1.2014v2$Ryegrass.cultivar=="Nui",]$reNDVI)

