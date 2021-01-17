################# Napa Vintage Dataset (PA) - Updated 12/2/2020 #########################

#Housekeeping
rm(list=ls())
options(stringsAsFactors = FALSE)

#Load libraries
library(dplyr)
library(lme4)

#Reading in csv files
mydat <- read.csv("/Users/phoebeautio/Desktop/Vintage Research/NapaComplete.csv", header=TRUE, na.strings=c(""," ","NA"))
head(mydat)

  mydat <- mydat[,c(1,10,11,12,13,2,3,4,6,7,8,5,9,14)] #reorganize columns

mydatsimp <- read.csv("/Users/phoebeautio/Desktop/Vintage Research/VintageTableForModelsSimp.csv", header=TRUE, na.strings=c(""," ","NA"))
head(mydatsimp)

  mydatsimp <- mydatsimp[,c(1,4,5,6,7,2,3)] #reorganize columns

#Rescaling precipitation (mm to cm)
  
#Modeling with lme4
  

#Fittedmodeling - "ranking = intercept + gdd * effect of gdd"
  
  "ranking = 73.97 + gdd * 0.0128, kinda means, ranking = 73.97 + gdd * 0"
  
  for(i in 1:length(varieties)){
    temp <- subset(mydat, Variety == varieties[i])
    mod.temp <- lm(R1_WS ~ gdd_avg * prcp_avg, data = temp) #complex, 4 things, prcp/gdd/effect of interaction/rank
    print(varieties[i]) 
    print(summary(mod.temp))}
  
  for(i in 1:length(varieties)){
    temp <- subset(mydat, Variety == varieties[i])
    mod.temp <- lm(R1_WS ~ gdd_avg + prcp_avg, data = temp) #simpler is better
    print(varieties[i])
    print(summary(mod.temp))}
  
chard_fittedmodel <- lm(Chardonnay_Table$R1_WS ~ Chardonnay_Table$gdd_avg * Chardonnay_Table$prcp_avg)
summary(chard_fittedmodel)  

#Subset by Variety 
  #variety
  Cabernet <- subset(mydat, mydat$Variety=="Cabernet")
  Chardonnay <- subset(mydat, mydat$Variety=="Chardonnay")
  Rhone <- subset(mydat, mydat$Variety=="Rhone")
  Merlot <- subset(mydat, mydat$Variety=="Merlot")
  Zinfandel <- subset(mydat, mydat$Variety=="Zinfandel")
  
#Histograms
  varieties <- unique(mydat$Variety)
  par(mfrow = c(2, 3))
  
  for(i in 1:length(varieties)){
    temp <- subset(mydat, Variety == varieties[i])
    hist(temp$gdd_avg, main = varieties[i])}
  
plot()
  