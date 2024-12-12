## GG Paper
# Federico Tomasetto 
# 2024/12/12

rm(list = ls(all.names = TRUE)) # will clear all objects, including hidden objects
gc()

library(tidyverse)
library(ggtext)

#Visualize CRW & Porina 
setwd("C:/Users/TOMASETTOF/OneDrive - AgResearch/ggj_paper/materials/GG data") 

library(readxl)

pasture_pest.df <- read_excel("Main - LURDF gg trial MRM - Copy.xlsx", sheet = "Sheet3")
glimpse(pasture_pest.df)

pasture_pest.df$`Sample Date` <-  format(as.Date(pasture_pest.df$`Sample Date`, format="%Y/%m/%d"),"%Y")

##PORINA
porina.df <- subset(pasture_pest.df, Species=="Porina")
glimpse(porina.df)

ggplot(porina.df, aes(x=`Sample Date`, y = `Mean m2`, fill = Cultivar)) + 
  geom_boxplot() + 
  theme_bw() +
  labs(fill = "Ryegrass cultivar", y= bquote(italic(Wiseana) ~ "larvae per" ~ m^2)) +
  geom_jitter(width=0.1, alpha=0.1) +
  labs(title = "Porina by cultivar per year", x = "Year") +
  facet_wrap(~`Sowing rate (kg/ha)`, labeller = labeller(`Sowing rate (kg/ha)` = c("6" = "Sowing rate = 6 kg/ha", "30" = "Sowing rate = 30 kg/ha")))

#CRW
crw.df <- subset(pasture_pest.df, Species=="CRW")
glimpse(crw.df)

ggplot(crw.df, aes(x=`Sample Date`, y = `Mean m2`, fill = Cultivar)) + 
  geom_boxplot() + 
  theme_bw() +
  labs(fill = "Ryegrass cultivar", y= bquote(italic(S.)~italic(obsoletus) ~ "larvae per" ~ m^2)) +
  geom_jitter(width=0.1, alpha=0.1) +
  labs(title = "CRW by cultivar per year", x = "Year") +
  facet_wrap(~`Sowing rate (kg/ha)`, labeller = labeller(`Sowing rate (kg/ha)` = c("6" = "Sowing rate = 6 kg/ha", "30" = "Sowing rate = 30 kg/ha")))
