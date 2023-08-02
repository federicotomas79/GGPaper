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

#Graphs for difference days between rs and gg sample
x11()
ggplot(gg1, aes(x = year, y = diff.days.sample, fill=diff.days.sample)) +
  geom_point(stat = 'identity', color="grey", show.legend = FALSE) +
  theme_bw() +
  labs(title = "Difference in days between gg sampling and rs data", y = "Difference in days (RS images)", x = "Sampling year")+
  stat_summary(fun.data=mean_sdl, fun.args = list(mult=1), geom="point", color="red", show.legend = FALSE) +
  scale_y_continuous(breaks = (c(0, -50, -100, -150, -200)), labels = c("Day GG sampled", -50, -100, -150, -200))



#Visualize grass grub densities by months
#range(gg1$Mean.m.2) #0-688 
ggplot(gg1, aes(x = gg.sample.days, y = Mean.m.2)) +
       geom_point(stat = 'identity', color="grey") +
       theme_bw() +
       scale_x_date(date_labels=("%d-%b-%Y"), 
                    breaks = unique(gg.sample.days)) +
       labs(title = "Grass grub per year", y = "Value", x = "Date") +
       stat_summary(fun.data=mean_sdl, fun.args = list(mult=1), geom="pointrange", color="red")

#Visualize grass grub densities by cultivar per year

x11()
ggplot(gg1, aes(x=factor(year), y = Mean.m.2, fill = Ryegrass.cultivar)) + 
  geom_boxplot() + 
  theme_bw() +
  labs(fill = "Ryegrass cultivar", y= expression("Larvae per"~ m^2)) +
  geom_jitter(width=0.1, alpha=0.1) +
  labs(title = "Grass grub by cultivar per year", x = "Year") +
  facet_wrap(~Sowing.rate..kg.ha., labeller = labeller(Sowing.rate..kg.ha. = c("6" = "Sowing rate = 6 kg/ha", "30" = "Sowing rate = 30 kg/ha")))

#Visualize grass grub infestation levels

gg1$gg_risk_label[gg1$gg_risk_label=="0"] <- "Low"
gg1$gg_risk_label[gg1$gg_risk_label=="1"] <- "High"

ggplot(gg1, aes(x=factor(gg_risk_label), y=Mean.m.2, color=factor(gg_risk_label))) + 
       geom_violin(trim=FALSE) +
       scale_x_discrete(limits = rev) +
       scale_color_brewer(palette="Set1") +
       theme_bw() +
       #stat_summary(fun=mean, geom="point", size=2) +
       guides(colour = "none") +    
       geom_jitter(width=0.1, alpha=0.1) +
       labs(title = "Grass grub by infestation levels", x = "Risk levels", y= expression("Larvae per"~ m^2)) +
       geom_boxplot(width=0.1)

ggplot(gg1, aes(x=factor(gg_risk_label), y=Mean.m.2, color=factor(gg_risk_label))) + 
       geom_violin(trim=FALSE) +
       scale_x_discrete(limits = rev) +
       scale_color_brewer(palette="Set1") +
       theme_bw() +
       #stat_summary(fun=mean, geom="point", size=2) +
       guides(colour = "none") +    
       geom_jitter(width=0.1, alpha=0.1) +
       labs(title = "Grass grub by infestation levels", x = "Risk levels", y= expression("Larvae per"~ m^2)) +
       geom_boxplot(width=0.1) +
       facet_wrap(~year)
  
#Create data table for exploration
library(DT)
gg1.datatable <- data.frame(gg1[,c(8:9,12:13,29)])
datatable(gg1.datatable, selection="multiple")

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

gg1.data <- data.frame(gg1[,c(9,16:25)]) 
x11()
ggpairs(gg1.data, title="Correlogram - Vegetation Indexes from Planet Lab vs. grass grub densities", upper = list(continuous = wrap("cor", size = 3)))

#p_title <- 'Vegetation Indeces from Planet Lab' 

#p1 <- ggplot() +
#      theme_bw() +
#      geom_point(data = gg1, aes(x = Blue, y = Mean.m.2), pch = 21, size = 3, alpha = .7, colour = 'grey50', fill = 'chartreuse4') +
#      geom_smooth(data = gg1, aes(x = Blue, y = Mean.m.2), method = 'lm') +
#      labs(title = p_title)+
#      facet_wrap(~year, ncol = 2)
