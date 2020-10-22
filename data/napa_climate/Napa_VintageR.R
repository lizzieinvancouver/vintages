################# Napa Vintage Dataset (PA) - 10/19/2020 #########################

#Notes: given some period of time, the output is summed GDD and/or summed precipitation 
#using GDD over the growing season to predict quality.... summation of precipitation between April 1st and sep 30th
#x year, y temp or precipitation

#Housekeeping
rm(list=ls())
options(stringsAsFactors = FALSE)

#Load libraries
library(lubridate)

#Reading in csv files
mydat <- read.csv("/Users/phoebeautio/Desktop/Vintage Research/Napa_Vintage.csv", header=TRUE, na.strings=c(""," ","NA"))
head(mydat)
climdat <- read.csv("/Users/phoebeautio/Desktop/Vintage Research/Napa_1990-2019.csv", header=TRUE, na.strings=c(""," ","NA"))

## Relative paths (Geoff)
## mydat <- read.csv("Napa_Vintage.csv", header=TRUE, na.strings=c(""," ","NA"))
## head(mydat)
## climdat <- read.csv("Napa_1990-2019.csv", header=TRUE, na.strings=c(""," ","NA"))

### Climate Data ###
#Parsing cliamte dates
climdat2 <- climdat #making duplicate
climdat2$year <- year(ymd(climdat2$DATE))
climdat2$month <- month(ymd(climdat2$DATE))
climdat2$day <- day(ymd(climdat2$DATE))

## Find intersecting years
years <- intersect(x = climdat2$year, y = mydat$Vintage)
## Subset both data sets by those years
climdat2 <- subset(climdat2, year %in% years)
mydat <- subset(mydat, Vintage %in% years)

## Which location?
unique(climdat2$NAME)

#Subsetting climate data (dates and columns of interest)
#April 1 - September 30
climdat3 <- climdat2[which(climdat2$month=="4" | climdat2$month=="5" | climdat2$month=="6" |
                             climdat2$month=="7" | climdat2$month=="8" | climdat2$month=="9"), ]

#Calculating Mean Temperature 
#right now some TOBS are lacking, and we are stil unsure of its validity
climdat3$TAVG <- rowMeans(climdat3[,c('TMAX', 'TMIN')], na.rm=TRUE)


### Vintage Data ###
#Subsetting by location (Napa/Sonoma)
napa <- mydat[which(mydat$Location=="Napa"), ]
  napa$Description_WS = NULL

sonoma <- mydat[which(mydat$Location=="Sonoma"), ]
  sonoma$Description_WS = NULL
  

### Calculating GDD -- GDD base temp = 10 ###
climdat3$gddbase <- ifelse(climdat3$TAVG >= 10, climdat3$TAVG - 10, 0)
climdat3$gdd <- ave(climdat3$gddbase, climdat3$year, FUN=cumsum)
  

### Calculating Precipitation ###
climdat3$prcpsum <- ave(climdat3$PRCP, climdat3$year, FUN=cumsum)


#Ailene GDD code: 
      
  #`If you have an object called daily_temp (TOBS) with daily temperature values:
  #specify the temperature threshold etc for forcing sum calculation (this is from a recent paper- happy to send a long the ):
    force = ifelse(climdat3$TOBS>5, 28.4/(1+exp(-.185*(climdat3$TOBS-18.4))),0)
  
  #add up all the forcing       
  forcesum <- sapply(1:ncol(force), function(x) (cumsum(force[1:nrow(force),x])))
  
  
  
