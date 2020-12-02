################# Napa Vintage Dataset (PA) - Updated 12/2/2020 #########################

#Housekeeping
rm(list=ls())
options(stringsAsFactors = FALSE)

#Load libraries
library(dplyr)

#Reading in csv files
mydat <- read.csv("/Users/phoebeautio/Desktop/Vintage Research/VintageTableForModels.csv", header=TRUE, na.strings=c(""," ","NA"))
head(mydat)

  mydat <- mydat[,c(1,10,11,12,13,2,3,4,6,7,8,5,9,14)] #reorganize columns

mydatsimp <- read.csv("/Users/phoebeautio/Desktop/Vintage Research/VintageTableForModelsSimp.csv", header=TRUE, na.strings=c(""," ","NA"))
head(mydatsimp)

  mydatsimp <- mydatsimp[,c(1,4,5,6,7,2,3)] #reorganize columns
  