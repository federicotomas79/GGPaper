## GG Paper
# Federico Tomasetto 
# 2024/12/12

rm(list = ls(all.names = TRUE)) # will clear all objects, including hidden objects
gc()

library(tidyverse)

#Visualize CRW & Porina 
setwd("C:/Users/TOMASETTOF/OneDrive - AgResearch/ggj_paper/materials/GG data") 

library(readxl)

pasture_pest.df <- read_excel("Main - LURDF gg trial MRM - Copy.xlsx", sheet = "Sheet3")
glimpse(pasture_pest.df)
