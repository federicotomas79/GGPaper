## GG Paper
# Federico Tomasetto 
# 2024/12/12

rm(list = ls(all.names = TRUE)) # will clear all objects, including hidden objects
gc()

library(tidyverse)

#Visualize CRW & Porina 
setwd("C:/Users/TOMASETTOF/OneDrive - AgResearch/ggj_paper/materials/GG data") 

library(readxl)

read_excel_allsheets <- function(filename, tibble = FALSE) {
  # I prefer straight data.frames
  # but if you like tidyverse tibbles (the default with read_excel)
  # then just pass tibble = TRUE
  sheets <- readxl::excel_sheets(filename)
  x <- lapply(sheets, function(X) readxl::read_excel(filename, sheet = X))
  if(!tibble) x <- lapply(x, as.data.frame)
  names(x) <- sheets
  x
}

mysheets <- read_excel_allsheets("Main - LURDF gg trial MRM - Copy.xlsx")
mysheets$`2 May 2012`$`Sample Date` <- as.Date(mysheets$`2 May 2012`$`Sample Date`, format = "%Y-%m-%d")
mysheets$`2 May 2014`$`Sample Date` <- as.Date(mysheets$`2 May 2014`$`Sample Date`, format = "%Y-%m-%d")
mysheets$`2 May 2015`$`Sample Date` <- as.Date(mysheets$`2 May 2015`$`Sample Date`, format = "%Y-%m-%d")
mysheets$`2 May 2016`$`Sample Date` <- as.Date(mysheets$`2 May 2016`$`Sample Date`, format = "%Y-%m-%d")
mysheets$`2 May 2017`$`Sample Date` <- as.Date(mysheets$`2 May 2017`$`Sample Date`, format = "%Y-%m-%d")

glimpse(mysheets)



mysheets$`2 May 2012`$Species=="Porina"
