################ Washing Vintage Dataset, Dealing with missing dates (PA) -- 5-6-2021 ################

#Housekeeping
rm(list=ls())
options(stringsAsFactors = FALSE)

#Load libraries
library(lubridate)
library(dplyr)
library(tidyr)

#Reading in csv files
climdat <- read.csv("/Users/phoebeautio/Desktop/Vintages/Data/NapaClimate2021/Washington/WAClim.csv", header=TRUE, na.strings=c(""," ","NA"))
mydat <- read.csv("/Users/phoebeautio/Desktop/Vintages/Data/NapaClimate2021/Washington/WA_Vintage.csv", header=TRUE, na.strings=c(""," ","NA"))
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
dayt <- subset(climdat, climdat$NAME=="DAYTON 1 WSW, WA US") #"DAYTON 1 WSW, WA US"***
dayt <- dayt[ order(dayt$year), ]
hat <- subset(climdat, climdat$NAME=="HATTON 9 SE, WA US") #HATTON 9 SE, WA US***
hat <- hat[ order(hat$year), ]
wal <- subset(climdat, climdat$NAME=="WALLA WALLA REGIONAL AIRPORT, WA US") #"WALLA WALLA REGIONAL AIRPORT, WA US"***
wal <- wal[ order(dayt$year), ]
eph <- subset(climdat, climdat$NAME=="EPHRATA AIRPORT, WA US") #"EPHRATA AIRPORT, WA US"***
eph <- eph[ order(hat$year), ]
dalle <- subset(climdat, climdat$NAME=="DALLESPORT AIRPORT, WA US") #"DALLESPORT AIRPORT, WA US"***
dalle <- dalle[ order(hat$year), ]

#removing empty rows
wal <- wal[!(is.na(wal$STATION) & is.na(wal$NAME) & is.na(wal$DATE)), ]

#correcting for singular NA TAVG values (averaging the day prior to and after the missing date)

#wal
for(i in 1:nrow(wal)){
  if(isTRUE(is.na(wal[i, "TAVG"]))) {
    print(i)
    wal$TAVG[i] <- sum(wal$TAVG[i-1], wal$TAVG[i+1]) / 2
  }
}

#eph
for(i in 1:nrow(eph)){
  if(isTRUE(is.na(eph[i, "TAVG"]))) {
    print(i)
    eph$TAVG[i] <- sum(eph$TAVG[i-1], eph$TAVG[i+1]) / 2
  }
}

#dalle
for(i in 1:nrow(dalle)){
  if(isTRUE(is.na(dalle[i, "TAVG"]))) {
    print(i)
    dalle$TAVG[i] <- sum(dalle$TAVG[i-1], dalle$TAVG[i+1]) / 2
  }
}

#dayt
for(i in 1:nrow(dayt)){
  if(isTRUE(is.na(dayt[i, "TAVG"]))) {
    print(i)
    dayt$TAVG[i] <- sum(dayt$TAVG[i-1], dayt$TAVG[i+1]) / 2
  }
}

#hat
for(i in 1:nrow(hat)){
  if(isTRUE(is.na(hat[i, "TAVG"]))) {
    print(i)
    hat$TAVG[i] <- sum(hat$TAVG[i-1], hat$TAVG[i+1]) / 2
  }
}

#averaged TAVG for each day at all stations. Will input into missing values
regional_TAVG <- aggregate(TAVG ~ DATE, climdat, mean)

#remaining NA TAVG that require regional averages
walNA_TAVG <- subset(wal, is.na(wal$TAVG))
ephNA_TAVG <- subset(eph, is.na(eph$TAVG))
daytNA_TAVG <- subset(dayt, is.na(dayt$TAVG))
hatNA_TAVG <- subset(hat, is.na(hat$TAVG))

#Finding intersecting dates
TAVG_dates_chr_wal <- intersect(x = walNA_TAVG$DATE, y = regional_TAVG$DATE)
TAVG_dates_wal <- subset(regional_TAVG, DATE %in% TAVG_dates_chr_wal)

TAVG_dates_chr_eph <- intersect(x = ephNA_TAVG$DATE, y = regional_TAVG$DATE)
TAVG_dates_eph <- subset(regional_TAVG, DATE %in% TAVG_dates_chr_eph)

TAVG_dates_chr_dayt <- intersect(x = daytNA_TAVG$DATE, y = regional_TAVG$DATE)
TAVG_dates_dayt <- subset(regional_TAVG, DATE %in% TAVG_dates_chr_dayt)

TAVG_dates_chr_hat <- intersect(x = hatNA_TAVG$DATE, y = regional_TAVG$DATE)
TAVG_dates_hat <- subset(regional_TAVG, DATE %in% TAVG_dates_chr_hat)

#substituting averaged dates into remaining NA values
for(i in 1:nrow(wal)){
  if(isTRUE(is.na(wal[i, "TAVG"]))) {
    print(i)
    date.index <- which(TAVG_dates_wal[, "DATE"] == wal[i, "DATE"]) 
    wal$TAVG[i] <- TAVG_dates_wal[date.index, "TAVG"] 
  }
}

for(i in 1:nrow(eph)){
  if(isTRUE(is.na(eph[i, "TAVG"]))) {
    print(i)
    date.index <- which(TAVG_dates_eph[, "DATE"] == eph[i, "DATE"]) 
    eph$TAVG[i] <- TAVG_dates_eph[date.index, "TAVG"] 
  }
}

for(i in 1:nrow(dayt)){
  if(isTRUE(is.na(dayt[i, "TAVG"]))) {
    print(i)
    date.index <- which(TAVG_dates_dayt[, "DATE"] == dayt[i, "DATE"]) 
    dayt$TAVG[i] <- TAVG_dates_dayt[date.index, "TAVG"] 
  }
}

for(i in 1:nrow(hat)){
  if(isTRUE(is.na(hat[i, "TAVG"]))) {
    print(i)
    date.index <- which(TAVG_dates_hat[, "DATE"] == hat[i, "DATE"]) 
    hat$TAVG[i] <- TAVG_dates_hat[date.index, "TAVG"] 
  }
}

#Correcting for PRCP NA values using regional averages
regional_PRCP <- aggregate(PRCP ~ DATE, climdat, mean)

#remaining NA PRCP that require regional averages
walNA_PRCP <- subset(wal, is.na(wal$PRCP))
ephNA_PRCP <- subset(eph, is.na(eph$PRCP))
dalleNA_PRCP <- subset(dalle, is.na(dalle$PRCP))
daytNA_PRCP <- subset(dayt, is.na(dayt$PRCP))
hatNA_PRCP <- subset(hat, is.na(hat$PRCP))

#Finding intersecting dates
PRCP_dates_wal_chr <- intersect(x = walNA_PRCP$DATE, y = regional_PRCP$DATE)
PRCP_dates_wal <- subset(regional_PRCP, DATE %in% PRCP_dates_wal_chr)

PRCP_dates_eph_chr <- intersect(x = ephNA_PRCP$DATE, y = regional_PRCP$DATE)
PRCP_dates_eph <- subset(regional_PRCP, DATE %in% PRCP_dates_eph_chr)

PRCP_dates_dalle_chr <- intersect(x = dalleNA_PRCP$DATE, y = regional_PRCP$DATE)
PRCP_dates_dalle <- subset(regional_PRCP, DATE %in% PRCP_dates_dalle_chr)

PRCP_dates_dayt_chr <- intersect(x = daytNA_PRCP$DATE, y = regional_PRCP$DATE)
PRCP_dates_dayt <- subset(regional_PRCP, DATE %in% PRCP_dates_dayt_chr)

PRCP_dates_hat_chr <- intersect(x = hatNA_PRCP$DATE, y = regional_PRCP$DATE)
PRCP_dates_hat <- subset(regional_PRCP, DATE %in% PRCP_dates_hat_chr)

#replacing NAs with regional values
#wal
for(i in 1:nrow(wal)){
  if(isTRUE(is.na(wal[i, "PRCP"]))) {
    print(i)
    date.index <- which(PRCP_dates_wal[, "DATE"] == wal[i, "DATE"])
    wal$PRCP[i] <- PRCP_dates_wal[date.index, "PRCP"]
  }
}

#eph
for(i in 1:nrow(eph)){
  if(isTRUE(is.na(eph[i, "PRCP"]))) {
    print(i)
    date.index <- which(PRCP_dates_eph[, "DATE"] == eph[i, "DATE"])
    eph$PRCP[i] <- PRCP_dates_eph[date.index, "PRCP"]
  }
}

#dalle
for(i in 1:nrow(dalle)){
  if(isTRUE(is.na(dalle[i, "PRCP"]))) {
    print(i)
    date.index <- which(PRCP_dates_dalle[, "DATE"] == dalle[i, "DATE"])
    dalle$PRCP[i] <- PRCP_dates_dalle[date.index, "PRCP"]
  }
}

#hat
for(i in 1:nrow(hat)){
  if(isTRUE(is.na(hat[i, "PRCP"]))) {
    print(i)
    date.index <- which(PRCP_dates_hat[, "DATE"] == hat[i, "DATE"])
    hat$PRCP[i] <- PRCP_dates_hat[date.index, "PRCP"]
  }
}

#dayt
for(i in 1:nrow(dayt)){
  if(isTRUE(is.na(dayt[i, "PRCP"]))) {
    print(i)
    date.index <- which(PRCP_dates_dayt[, "DATE"] == dayt[i, "DATE"])
    dayt$PRCP[i] <- PRCP_dates_dayt[date.index, "PRCP"]
  }
}

#write csv
write.csv(dayt,"/Users/phoebeautio/Desktop/Vintages/Data/NapaClimate2021/Washington/DaytClean.csv", row.names = FALSE)
write.csv(hat,"/Users/phoebeautio/Desktop/Vintages/Data/NapaClimate2021/Washington/HatClean.csv", row.names = FALSE)
write.csv(wal,"/Users/phoebeautio/Desktop/Vintages/Data/NapaClimate2021/Washington/WalClean.csv", row.names = FALSE)
write.csv(eph,"/Users/phoebeautio/Desktop/Vintages/Data/NapaClimate2021/Washington/EphClean.csv", row.names = FALSE)
write.csv(dalle,"/Users/phoebeautio/Desktop/Vintages/Data/NapaClimate2021/Washington/DalleClean.csv", row.names = FALSE)