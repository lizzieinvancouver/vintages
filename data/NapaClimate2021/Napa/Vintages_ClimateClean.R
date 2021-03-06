################ Napa Vintage Dataset, Dealing with missing dates (PA) - 1-17-2020 ################

#Housekeeping
rm(list=ls())
options(stringsAsFactors = FALSE)

#Load libraries
library(lubridate)
library(dplyr)
library(tidyr)

#Reading in csv files
climdat_og <- read.csv("/Users/phoebeautio/Desktop/Vintage Research/Napa_1990-2019.csv", header=TRUE, na.strings=c(""," ","NA"))
head(climdat_og)
mydat <- read.csv("/Users/phoebeautio/Desktop/Vintage Research/Napa_Vintage.csv", header=TRUE, na.strings=c(""," ","NA"))
head(mydat)

#Parsing cliamte dates
climdat <- climdat_og #making duplicate
climdat$year <- year(ymd(climdat$DATE))
climdat$month <- month(ymd(climdat$DATE))
climdat$day <- day(ymd(climdat$DATE))

#Removing unused columns
climdat$DAPR <- NULL
climdat$MDPR <- NULL
climdat$SNOW <- NULL
climdat$SNWD <- NULL

#Deleting year 1990 as one station is missing the month of May
climdat <- climdat[!(climdat$year=="1990"), ]

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
st_hosp <- subset(climdat, climdat$STATION=="USC00046074") #NAPA STATE HOSPITAL***
st_helena <- subset(climdat, climdat$STATION=="USC00047643") #SAINT HELENA***

#correcting for singular NA TAVG values (averaging the day prior to and after the missing date)
#st_hosp
for(i in 1:nrow(st_hosp)){
  if(isTRUE(is.na(st_hosp[i, "TAVG"]))) {
    print(i)
    st_hosp$TAVG[i] <- sum(st_hosp$TAVG[i-1], st_hosp$TAVG[i+1]) / 2
  }
}

#st_helena
for(i in 1:nrow(st_helena)){
  if(isTRUE(is.na(st_helena[i, "TAVG"]))) {
    print(i)
    st_helena$TAVG[i] <- sum(st_helena$TAVG[i-1], st_helena$TAVG[i+1]) / 2
  }
}

#averaged TAVG for each day at all stations. Will input into missing st_helena values
  regional_TAVG <- aggregate(TAVG ~ DATE, climdat, mean)
  
#remaining NA TAVG that require regional averages
  helenaNA_TAVG <- subset(st_helena, is.na(st_helena$TAVG))
    
#Finding intersecting dates
  TAVG_dates_chr <- intersect(x = helenaNA_TAVG$DATE, y = regional_TAVG$DATE)
  TAVG_dates <- subset(regional_TAVG, DATE %in% TAVG_dates_chr)
    
#substituting averaged dates into remaining NA values
    for(i in 1:nrow(st_helena)){
      if(isTRUE(is.na(st_helena[i, "TAVG"]))) {
        print(i)
        date.index <- which(TAVG_dates[, "DATE"] == st_helena[i, "DATE"]) 
        st_helena$TAVG[i] <- TAVG_dates[date.index, "TAVG"] 
      }
    }

#Correcting for PRCP NA values using regional averages
  regional_PRCP <- aggregate(PRCP ~ DATE, climdat, mean)
  
  #remaining NA TAVG that require regional averages
  helenaNA_PRCP <- subset(st_helena, is.na(st_helena$PRCP))
  hospNA_PRCP <- subset(st_hosp, is.na(st_hosp$PRCP))
  
  #Finding intersecting dates
  PRCP_dates_helena_chr <- intersect(x = helenaNA_PRCP$DATE, y = regional_PRCP$DATE)
  PRCP_dates_helena <- subset(regional_PRCP, DATE %in% PRCP_dates_helena_chr)
  
  PRCP_dates_hosp_chr <- intersect(x = hospNA_PRCP$DATE, y = regional_PRCP$DATE)
  PRCP_dates_hosp <- subset(regional_PRCP, DATE %in% PRCP_dates_hosp_chr)

#st_hosp
for(i in 1:nrow(st_hosp)){
  if(isTRUE(is.na(st_hosp[i, "PRCP"]))) {
    print(i)
    date.index <- which(PRCP_dates_hosp[, "DATE"] == st_hosp[i, "DATE"])
    st_hosp$PRCP[i] <- PRCP_dates_hosp[date.index, "PRCP"]
  }
}

#st_helena
for(i in 1:nrow(st_helena)){
  if(isTRUE(is.na(st_helena[i, "PRCP"]))) {
    print(i)
    date.index <- which(PRCP_dates_helena[, "DATE"] == st_helena[i, "DATE"])
    st_helena$PRCP[i] <- PRCP_dates_helena[date.index, "PRCP"]
  }
}

#write csv
write.csv(st_helena,"/Users/phoebeautio/Desktop/Vintage Research/StHelenaClean.csv", row.names = FALSE)
write.csv(st_hosp,"/Users/phoebeautio/Desktop/Vintage Research/StHospClean.csv", row.names = FALSE)

