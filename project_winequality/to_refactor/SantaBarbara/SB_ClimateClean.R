################ Santa Barbara Vintage Dataset, Dealing with missing dates (PA) -- 3-18-2021 ################

#Housekeeping
rm(list=ls())
options(stringsAsFactors = FALSE)

#Load libraries
library(lubridate)
library(dplyr)
library(tidyr)

#Reading in csv files
climdat <- read.csv("/Users/phoebeautio/Desktop/Vintage Research/SBClimate.csv", header=TRUE, na.strings=c(""," ","NA"))
mydat <- read.csv("/Users/phoebeautio/Desktop/Vintage Research/SB_Vintage.csv", header=TRUE, na.strings=c(""," ","NA"))
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
smaria_air <- subset(climdat, climdat$NAME=="SANTA MARIA PUBLIC AIRPORT, CA US") #SANTA MARIA PUBLIC AIRPORT, CA US***
sb <- subset(climdat, climdat$NAME=="SANTA BARBARA, CA US") #SANTA BARBARA, CA US***
#lompoc <- subset(climdat, climdat$NAME=="LOMPOC, CA US") #LOMPOC, CA US***

#correcting for singular NA TAVG values (averaging the day prior to and after the missing date)
#smaria_air
for(i in 1:nrow(smaria_air)){
  if(isTRUE(is.na(smaria_air[i, "TAVG"]))) {
    print(i)
    smaria_air$TAVG[i] <- sum(smaria_air$TAVG[i-1], smaria_air$TAVG[i+1]) / 2
  }
}

#sb
for(i in 1:nrow(sb)){
  if(isTRUE(is.na(sb[i, "TAVG"]))) {
    print(i)
    sb$TAVG[i] <- sum(sb$TAVG[i-1], sb$TAVG[i+1]) / 2
  }
}

#averaged TAVG for each day at all stations. Will input into missing sb values
regional_TAVG <- aggregate(TAVG ~ DATE, climdat, mean)

#remaining NA TAVG that require regional averages
sbNA_TAVG <- subset(sb, is.na(sb$TAVG))

#Finding intersecting dates
TAVG_dates_chr <- intersect(x = sbNA_TAVG$DATE, y = regional_TAVG$DATE)
TAVG_dates <- subset(regional_TAVG, DATE %in% TAVG_dates_chr)

#substituting averaged dates into remaining NA values
for(i in 1:nrow(sb)){
  if(isTRUE(is.na(sb[i, "TAVG"]))) {
    print(i)
    date.index <- which(TAVG_dates[, "DATE"] == sb[i, "DATE"]) 
    sb$TAVG[i] <- TAVG_dates[date.index, "TAVG"] 
  }
}

#Correcting for PRCP NA values using regional averages
regional_PRCP <- aggregate(PRCP ~ DATE, climdat, mean)

#remaining NA PRCP that require regional averages
sbNA_PRCP <- subset(sb, is.na(sb$PRCP))
smariaNA_PRCP <- subset(smaria_air, is.na(smaria_air$PRCP))

#Finding intersecting dates
PRCP_dates_sb_chr <- intersect(x = sbNA_PRCP$DATE, y = regional_PRCP$DATE)
PRCP_dates_sb <- subset(regional_PRCP, DATE %in% PRCP_dates_sb_chr)

PRCP_dates_smaria_chr <- intersect(x = smariaNA_PRCP$DATE, y = regional_PRCP$DATE)
PRCP_dates_smaria <- subset(regional_PRCP, DATE %in% PRCP_dates_smaria_chr)

#smaria_air
for(i in 1:nrow(smaria_air)){
  if(isTRUE(is.na(smaria_air[i, "PRCP"]))) {
    print(i)
    date.index <- which(PRCP_dates_smaria[, "DATE"] == smaria_air[i, "DATE"])
    smaria_air$PRCP[i] <- PRCP_dates_smaria[date.index, "PRCP"]
  }
}

#sb
for(i in 1:nrow(sb)){
  if(isTRUE(is.na(sb[i, "PRCP"]))) {
    print(i)
    date.index <- which(PRCP_dates_sb[, "DATE"] == sb[i, "DATE"])
    sb$PRCP[i] <- PRCP_dates_sb[date.index, "PRCP"]
  }
}

#write csv
write.csv(sb,"/Users/phoebeautio/Desktop/Vintage Research/SBClean.csv", row.names = FALSE)
write.csv(smaria_air,"/Users/phoebeautio/Desktop/Vintage Research/SMariaClean.csv", row.names = FALSE)
