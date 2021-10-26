################ Oregon Vintage Dataset, Dealing with missing dates (PA) -- 3-18-2021 ################

#Housekeeping
rm(list=ls())
options(stringsAsFactors = FALSE)

#Load libraries
library(lubridate)
library(dplyr)
library(tidyr)

#Reading in csv files
climdat <- read.csv("/Users/phoebeautio/Desktop/Vintage Research/ORClimate.csv", header=TRUE, na.strings=c(""," ","NA"))
mydat <- read.csv("/Users/phoebeautio/Desktop/Vintage Research/OR_Vintage.csv", header=TRUE, na.strings=c(""," ","NA"))
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
climdat <- climdat[ order(climdat$NAME), ]

#Subsetting locations
sal_air <- subset(climdat, climdat$NAME=="SALEM AIRPORT MCNARY FIELD, OR US") #SALEM AIRPORT MCNARY FIELD, OR US***
sal_air <- sal_air[ order(sal_air$year), ]
sil <- subset(climdat, climdat$NAME=="SILVERTON, OR US") #SILVERTON, OR US***
sil <- sil[ order(sil$year), ]

#correcting for singular NA TAVG values (averaging the day prior to and after the missing date)
#sal_air doens't have any NAs

#sil
for(i in 1:nrow(sil)){
  if(isTRUE(is.na(sil[i, "TAVG"]))) {
    print(i)
    sil$TAVG[i] <- sum(sil$TAVG[i-1], sil$TAVG[i+1]) / 2
  }
}

#averaged TAVG for each day at all stations. Will input into missing sil values
regional_TAVG <- aggregate(TAVG ~ DATE, climdat, mean)

#remaining NA TAVG that require regional averages
silNA_TAVG <- subset(sil, is.na(sil$TAVG))

#Finding intersecting dates
TAVG_dates_chr <- intersect(x = silNA_TAVG$DATE, y = regional_TAVG$DATE)
TAVG_dates <- subset(regional_TAVG, DATE %in% TAVG_dates_chr)

#substituting averaged dates into remaining NA values
for(i in 1:nrow(sil)){
  if(isTRUE(is.na(sil[i, "TAVG"]))) {
    print(i)
    date.index <- which(TAVG_dates[, "DATE"] == sil[i, "DATE"]) 
    sil$TAVG[i] <- TAVG_dates[date.index, "TAVG"] 
  }
}

#Correcting for PRCP NA values using regional averages
regional_PRCP <- aggregate(PRCP ~ DATE, climdat, mean)

#remaining NA PRCP that require regional averages
silNA_PRCP <- subset(sil, is.na(sil$PRCP))
sal_airNA_PRCP <- subset(sal_air, is.na(sal_air$PRCP))

#Finding intersecting dates
PRCP_dates_sil_chr <- intersect(x = silNA_PRCP$DATE, y = regional_PRCP$DATE)
PRCP_dates_sil <- subset(regional_PRCP, DATE %in% PRCP_dates_sil_chr)

PRCP_dates_sal_chr <- intersect(x = sal_airNA_PRCP$DATE, y = regional_PRCP$DATE)
PRCP_dates_sal <- subset(regional_PRCP, DATE %in% PRCP_dates_sal_chr)

#sal_air
for(i in 1:nrow(sal_air)){
  if(isTRUE(is.na(sal_air[i, "PRCP"]))) {
    print(i)
    date.index <- which(PRCP_dates_sal[, "DATE"] == sal_air[i, "DATE"])
    sal_air$PRCP[i] <- PRCP_dates_sal[date.index, "PRCP"]
  }
}

#sil
for(i in 1:nrow(sil)){
  if(isTRUE(is.na(sil[i, "PRCP"]))) {
    print(i)
    date.index <- which(PRCP_dates_sil[, "DATE"] == sil[i, "DATE"])
    sil$PRCP[i] <- PRCP_dates_sil[date.index, "PRCP"]
  }
}

#write csv
write.csv(sil,"/Users/phoebeautio/Desktop/Vintage Research/SilClean.csv", row.names = FALSE)
write.csv(sal_air,"/Users/phoebeautio/Desktop/Vintage Research/SalAirClean.csv", row.names = FALSE)
