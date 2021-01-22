################# Napa Vintage Dataset (PA) - Updated 12/2/2020 #########################

#Housekeeping
rm(list=ls())
options(stringsAsFactors = FALSE)

#Load libraries
library(dplyr)
library(lme4)
library(tidyr)

## #Reading in csv files
## mydat <- read.csv("/Users/phoebeautio/Desktop/Vintage Research/TablesForModels/NapaComplete_phen.csv", header=TRUE, na.strings=c(""," ","NA"))
## head(mydat)
## #creating columns for each event
## mydat <- pivot_wider(mydat, names_from = phen_stage, values_from = c(st_helena_prcp, st_hosp_prcp, st_helena_gdd, st_hosp_gdd,
##                                                                     gdd_avg_phen, prcp_avg_phen))

## mydat <- mydat[,c(1,2,3,4,5,6,10,11,9,13,14,12,16,17,15,19,20,18,22,23,21,25,26,24,7,8)] #reorganize columns

## Geoff's file path
mydat <- read.csv("NapaComplete2_phen.csv",header = TRUE)
## Remove description column
mydat <- mydat[, -c(25)]

## Fit linear model to data

### Model 1 (just GDDs)
model1 <- lm(R1_WS ~ gdd_avg_phen_1 + gdd_avg_phen_2 + gdd_avg_phen_3, data = mydat)
#### Summarize fit
summary(model1)

### Model 2 (just prcp)

### Model 3 (GDD and prcp)

### Model 4 (interaction)
model4 <- lm(R1_WS ~ gdd_avg_phen_1 * prcp_avg_phen_1, data = mydat)

### Next step - lmer (Random intercept model (variety))










## Plotting
stages <- sort(unique(mydat$phen_stage))
varieties <- unique(mydat$Variety)

## Temperature plots
par(mfrow = c(length(stages), length(varieties)))
for(i in 1:length(stages)){
  for(j in 1:length(varieties)){
    temp <- subset(mydat, mydat$phen_stage == stages[i] & mydat$Variety == varieties[j])
    print(temp)
    plot(R1_WS ~ gdd_avg_phen, data = temp, main = varieties[j])
  }
}

## Precipitation plots
par(mfrow = c(length(stages), length(varieties)))
for(i in 1:length(stages)){
  for(j in 1:length(varieties)){
    temp <- subset(mydat, mydat$phen_stage == stages[i] & mydat$Variety == varieties[j])
    print(temp)
    plot(R1_WS ~ prcp_avg_phen, data = temp, main = varieties[j])
  }
}

## Linear models


#Pivoting Table 



######### OLD CODE


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
