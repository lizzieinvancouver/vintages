################ Sonoma Vintage Dataset, Dealing with missing dates (PA) -- 3-14-2021 ################

#Housekeeping
rm(list=ls())
options(stringsAsFactors = FALSE)

#Load libraries
library(lubridate)
library(dplyr)
library(tidyr)

#Reading in csv files
climdat <- read.csv("/Users/phoebeautio/Desktop/Vintage Research/SonomaClimate.csv", header=TRUE, na.strings=c(""," ","NA"))
mydat <- read.csv("/Users/phoebeautio/Desktop/Vintage Research/Napa_Vintage.csv", header=TRUE, na.strings=c(""," ","NA"))
head(mydat)

#Parsing cliamte dates
climdat$year <- year(ymd(climdat$DATE))
climdat$month <- month(ymd(climdat$DATE))
climdat$day <- day(ymd(climdat$DATE))

#Finding intersecting years
years <- intersect(x = climdat$year, y = mydat$Vintage)

#Subset both data sets by those years
climdat <- subset(climdat, year %in% years)
mydat <- subset(mydat, Vintage %in% years)

#Subsetting climate data (dates and columns of interest)
#April 1 - September 30
climdat <- climdat[which(climdat$month=="3" | climdat$month=="4" | climdat$month=="5" | climdat$month=="6" |
                           climdat$month=="7" | climdat$month=="8" | climdat$month=="9"), ]

#Calculating Mean Temperature 
climdat$TAVG <- rowMeans(climdat[c('TMAX', 'TMIN')], na.rm=FALSE)

#Subsetting locations
p_air <- subset(climdat, climdat$NAME=="PETALUMA AIRPORT, CA US") #PETALUMA AIRPORT, CA US***

#correcting for singular NA TAVG values (averaging the day prior to and after the missing date)
#p_air
for(i in 1:nrow(p_air)){
  if(isTRUE(is.na(p_air[i, "TAVG"]))) {
    print(i)
      p_air$TAVG[i] <- sum(p_air$TAVG[i-1], p_air$TAVG[i+1]) / 2
  }
}

for(i in 1:nrow(p_air)){
  if(isTRUE(is.na(p_air[i, "TAVG"]))) {
    print(i)
  }
}

#averaged TAVG for each day at all stations. Will input into missing st_helena values
regional_TAVG <- aggregate(TAVG ~ DATE, climdat, mean)

#remaining NA TAVG that require regional averages
pairNA_TAVG <- subset(p_air, is.na(p_air$TAVG))

#Finding intersecting dates
TAVG_dates_chr <- intersect(x = pairNA_TAVG$DATE, y = regional_TAVG$DATE)
TAVG_dates <- subset(regional_TAVG, DATE %in% TAVG_dates_chr)

#substituting averaged dates into remaining NA values
for(i in 1:nrow(p_air)){
  if(isTRUE(is.na(p_air[i, "TAVG"]))) {
    print(i)
    date.index <- which(TAVG_dates[, "DATE"] == p_air[i, "DATE"]) 
    p_air$TAVG[i] <- TAVG_dates[date.index, "TAVG"] 
  }
}

#Correcting for PRCP NA values (using regional averages)
regional_PRCP <- aggregate(PRCP ~ DATE, climdat, mean)

#remaining NA PRCP that require regional averages
pairNA_PRCP <- subset(p_air, is.na(p_air$PRCP))

#Finding intersecting dates
PRCP_dates_pair_chr <- intersect(x = pairNA_PRCP$DATE, y = regional_PRCP$DATE)
PRCP_dates_pair <- subset(regional_PRCP, DATE %in% PRCP_dates_pair_chr)

#p_air
for(i in 1:nrow(p_air)){
  if(isTRUE(is.na(p_air[i, "PRCP"]))) {
    print(i)
    date.index <- which(PRCP_dates_pair[, "DATE"] == p_air[i, "DATE"])
    p_air$PRCP[i] <- PRCP_dates_pair[date.index, "PRCP"]
  }
}

#write csv
write.csv(p_air,"/Users/phoebeautio/Desktop/Vintage Research/SonomaClimateClean.csv", row.names = FALSE)
