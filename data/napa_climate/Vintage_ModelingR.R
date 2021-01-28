################# Napa Vintage Dataset (PA) - Updated 1/28/2021 #########################

#Housekeeping
rm(list=ls())
options(stringsAsFactors = FALSE)

#Load libraries
library(dplyr)
library(lme4)
library(tidyr)
library(measurements)

#Reading in csv files
mydat <- read.csv("/Users/phoebeautio/Desktop/Vintage Research/TablesForModels/NapaComplete_phen.csv", header=TRUE, na.strings=c(""," ","NA"))
head(mydat)

sonoma_phen <- read.csv("/Users/phoebeautio/Desktop/Vintage Research/TablesForModels/SonomaComplete_phen.csv", header=TRUE, na.strings=c(""," ","NA"))
head(sonoma_phen)

nc_phen <- read.csv("/Users/phoebeautio/Desktop/Vintage Research/TablesForModels/NorthCoastComplete_phen.csv", header=TRUE, na.strings=c(""," ","NA"))
head(nc_phen)


## Geoff's file path
mydat <- read.csv("TablesForModels/NapaComplete_phen.csv",header = TRUE)
sonoma_phen <- read.csv("TablesForModels/SonomaComplete_phen.csv", header = TRUE)
nc_phen <- read.csv("TablesForModels/NorthCoastComplete_phen.csv", header = TRUE)

#Remove description column
mydat <- mydat[, -c(25)]
sonoma_phen <- sonoma_phen[, -c(25)]
nc_phen <- nc_phen[, -c(25)]


## Fit linear model to data ##

### Model 1 (just GDDs)
model1 <- lm(R1_WS ~ gdd_avg_phen_1 + gdd_avg_phen_2 + gdd_avg_phen_3, data = mydat)
#### Summarize fit
summary(model1)

### Model 2 (just prcp)
model2 <- lm(R1_WS ~ prcp_avg_phen_1 + prcp_avg_phen_2 + prcp_avg_phen_3, data = mydat)
#### Summarize fit
summary(model2)

### Model 3 (GDD and prcp)
model3 <- lm(R1_WS ~ gdd_avg_phen_1 + gdd_avg_phen_2 + gdd_avg_phen_3 + prcp_avg_phen_1 + prcp_avg_phen_2 + prcp_avg_phen_3, data = mydat)
#### Summarize fit
summary(model3)

### Model 4 (interaction, each stage seperate)
model4 <- lm(R1_WS ~ gdd_avg_phen_1 * prcp_avg_phen_1, data = mydat)
#### Summarize fit
summary(model4)

model5 <- lm(R1_WS ~ gdd_avg_phen_2 * prcp_avg_phen_2, data = mydat)
#### Summarize fit
summary(model5)

model6 <- lm(R1_WS ~ gdd_avg_phen_3 * prcp_avg_phen_3, data = mydat)
#### Summarize fit
summary(model6)

### Next step - lmer (Random intercept model (variety))

###rescaling prcp data (mm to cm)
prcp1 <- conv_unit(mydat$prcp_avg_phen_1, "mm", "cm")
mydat$prcp_avg_phen_1 <- paste(prcp1)
prcp2 <- conv_unit(mydat$prcp_avg_phen_2, "mm", "cm")
mydat$prcp_avg_phen_2 <- paste(prcp2)
prcp3 <- conv_unit(mydat$prcp_avg_phen_3, "mm", "cm")
mydat$prcp_avg_phen_3 <- paste(prcp3)

### Random intercept model
### Rank ~ Normal(mean = Intercept_variety, standard deviation = sigma) 
model_interceptonly <- lmer(R1_WS ~ (1 | Variety), data = mydat)

### Random intercept and slope
### Rank ~ Normal(mean = Intercept_variety + Effect_gdd1_variety * gdd1, standard deviation = sigma)
model_interceptslope <- lmer(R1_WS ~ (gdd_avg_phen_1 | Variety), data = mydat)

### Random intercept model, with 3 fixed effects
### rank ~ Normal(mean = Intercept_variety + (Effect_gdd1 * gdd1) + (Effect_gdd2 * gdd2) + (Effect_gdd3 * gdd3), standard deviation = sigma)
model_allthree <- lmer(R1_WS ~ (1 | Variety) + gdd_avg_phen_1 + gdd_avg_phen_2 + gdd_avg_phen_3, data = mydat)
summary(model_allthree)
confint(model_allthree) ### what are the 95% confidence intervals?
ranef(model_allthree) ### what are the variety-level "offsets" from the intercept?


coef(summary(m)) #fixed effects estimates
VarCorr(m) #figuring out correct random effects?
str(resid(m))


######### OLD CODE

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
