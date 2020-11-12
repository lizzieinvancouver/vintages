################# Napa Vintage Dataset (PA) - Updated 11/11/2020 #########################

#Housekeeping
rm(list=ls())
options(stringsAsFactors = FALSE)

#Load libraries
library(lubridate)
library(ggplot2)

#Reading in csv files
mydat <- read.csv("/Users/phoebeautio/Desktop/Vintage Research/Napa_Vintage.csv", header=TRUE, na.strings=c(""," ","NA"))
head(mydat)
climdat_og <- read.csv("/Users/phoebeautio/Desktop/Vintage Research/Napa_1990-2019.csv", header=TRUE, na.strings=c(""," ","NA"))

## Relative paths (Geoff)
## mydat <- read.csv("Napa_Vintage.csv", header=TRUE, na.strings=c(""," ","NA"))
## head(mydat)
## climdat_og <- read.csv("Napa_1990-2019.csv", header=TRUE, na.strings=c(""," ","NA"))

### Climate Data ###
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

#Finding intersecting years
years <- intersect(x = climdat$year, y = mydat$Vintage)

#Subset both data sets by those years
climdat <- subset(climdat, year %in% years)
mydat <- subset(mydat, Vintage %in% years)

#Subsetting climate data (dates and columns of interest)
#April 1 - September 30
climdat <- climdat[which(climdat$month=="4" | climdat$month=="5" | climdat$month=="6" |
                             climdat$month=="7" | climdat$month=="8" | climdat$month=="9"), ]

#Calculating Mean Temperature 
#right now some TOBS are lacking, and we are stil unsure of its validity
climdat$TAVG <- rowMeans(climdat[,c('TMAX', 'TMIN')], na.rm=FALSE)

plot(TAVG~TOBS, 
     data = climdat, 
     xlim = c(-20, 40), 
     ylim = c(-20, 40)) #positively correlated, tight near top and bottom but not in middle. One cluster. 

###subset by location and year to test trajectories
## Subsetting locations
unique(climdat$NAME)
unique(climdat$STATION)

st_hosp <- subset(climdat, climdat$STATION=="USC00046074") #NAPA STATE HOSPITAL***
st_helena <- subset(climdat, climdat$STATION=="USC00047643") #SAINT HELENA***
st_apuc <- subset(climdat, climdat$STATION=="USC00040212") #ANGWIN PACIFIC UNION COLLEGE***

#USC00045360 <- subset(climdat, climdat$STATION=="USC00045360") #MARKLEY COVE*** #near a lake (influenced by micro climates) and far from wineries
#US1CANP0003 <- subset(climdat, climdat$STATION=="US1CANP0003") #CALISTOGA 0.4 SSE **
#US1CANP0005 <- subset(climdat, climdat$STATION=="US1CANP0005") #NAPA 2.0 WNW #very south
#USC00041312 <- subset(climdat, climdat$STATION=="USC00041312") #CALISTOGA **
#US1CANP0006 <- subset(climdat, climdat$STATION=="US1CANP0006") #AMERICAN CANYON 3.5 NE
#USC00047646 <- subset(climdat, climdat$STATION=="USC00047646") #SAINT HELENA 4 WSW
#US1CANP0010 <- subset(climdat, climdat$STATION=="US1CANP0010") #NAPA 2.0 E
#USC00049859 <- subset(climdat, climdat$STATION=="USC00049859") #YOUNTVILLE
#US1CANP0007 <- subset(climdat, climdat$STATION=="US1CANP0007") #ST. HELENA 1.1 S
#USC00044673 <- subset(climdat, climdat$STATION=="USC00044673") #LAKE BERRYESSA
#USW00093227 <- subset(climdat, climdat$STATION=="USW00093227") #NAPA CO AIRPORT
#US1CANP0008 <- subset(climdat, climdat$STATION=="US1CANP0008") #AMERICAN CANYON 0.3 S

#Plotting 3 largest datasets to view tragectory
  #Finding intersecting dates
  dates <- intersect(x = st_helena$DATE, y = st_apuc$DATE)
  dates2 <- intersect(x = st_helena$DATE, y = st_hosp$DATE)

  #Subset both data sets by those years
  st_apuc2 <- subset(st_apuc, DATE %in% dates)
  st_helena2 <- subset(st_helena, DATE %in% dates)
  st_hosp2 <- subset(st_hosp, DATE %in% dates2) #not subsetting correctly?
  st_helena3 <- subset(st_helena, DATE %in% dates2)

plot(st_helena2$TAVG ~ st_apuc2$TAVG,
     col="blue",
     xlab="st_helena TAVG", 
     ylab="st_apuc TAVG", 
     main = "Saint Helena vs. Angwin Pacific Union College")

abline(lm(st_helena2$TAVG~st_apuc2$TAVG), col="black")

plot(st_helena3$TAVG ~ st_hosp2$TAVG,
     col="blue",
     xlab="st_helena TAVG", 
     ylab="st_hosp TAVG", 
     main = "TAVG Tragectory, Saint Helena vs. Napa State Hospital")

abline(lm(st_helena3$TAVG~st_hosp2$TAVG), col="black")

#Calculating GDD at each location -- GDD base temp = 10
climdat$gddbase <- ifelse(climdat$TAVG >= 10, climdat$TAVG - 10, 0)
climdat$gdd <- ave(climdat$gddbase, climdat$year, FUN=cumsum)

st_apuc$gddbase <- ifelse(st_apuc$TAVG >= 10, st_apuc$TAVG - 10, 0)
st_apuc$gdd <- ave(st_apuc$gddbase, st_apuc$year, FUN=cumsum)

st_helena$gddbase <- ifelse(st_helena$TAVG >= 10, st_helena$TAVG - 10, 0)
st_helena$gdd <- ave(st_helena$gddbase, st_helena$year, FUN=cumsum)

st_hosp$gddbase <- ifelse(st_hosp$TAVG >= 10, st_hosp$TAVG - 10, 0)
st_hosp$gdd <- ave(st_hosp$gddbase, st_hosp$year, FUN=cumsum)

###creating dataframe of agg gdd from each year and each location###
  # These dataframes come back mostly empty as a result of NA values in the gdd calculations. 
  # An NA affects the summation and produces an NA for the entire year.
  # filling it with a 0 would likely skew the data, and I am unsure about how to calculate an appropriate average

#ag <- aggregate(climdat$gddbase, by = list(climdat$year), FUN=sum) #doesnt work for each year, need to use on dataframe of annual gdd
#colnames(ag)[1] <- c("Vintage")

ag_st_apuc <- aggregate(st_apuc$gddbase, by = list(st_apuc$year), FUN=sum) 
colnames(ag_st_apuc)[1] <- c("Vintage")

ag_st_helena <- aggregate(st_helena$gddbase, by = list(st_helena$year), FUN=sum) 
colnames(st_helena)[1] <- c("Vintage")

ag_st_hosp <- aggregate(st_hosp$gddbase, by = list(st_hosp$year), FUN=sum) 
colnames(ag_st_hosp)[1] <- c("Vintage")

#Calculating Precipitation at each location
climdat$prcpsum <- ave(climdat$PRCP, climdat$year, FUN=cumsum)
st_apuc$prcpsum <- ave(st_apuc$PRCP, st_apuc$year, FUN=cumsum)
st_helena$prcpsum <- ave(st_helena$PRCP, st_helena$year, FUN=cumsum)
st_hosp$prcpsum <- ave(st_hosp$PRCP, st_hosp$year, FUN=cumsum)


### Vintage Data ###
#Subsetting by location (Napa/Sonoma)
napa <- mydat[which(mydat$Location=="Napa"), ]
  napa$Description_WS = NULL

sonoma <- mydat[which(mydat$Location=="Sonoma"), ]
  sonoma$Description_WS = NULL

#Editing Rhône character
mydat$Variety[which(mydat$Variety=="Rhône-Style Reds")] <- "Rhone Style Reds"
  
#Subsetting by Variety
unique(mydat$Variety)

Cabernet <- subset(mydat, mydat$Variety=="Cabernet")
Chardonnay <- subset(mydat, mydat$Variety=="Chardonnay")
Rhone <- subset(mydat, mydat$Variety=="Rhone Style Reds")
Merlot <- subset(mydat, mydat$Variety=="Merlot")
Zinfandel <- subset(mydat, mydat$Variety=="Zinfandel")

#Merging Varieties with the ag_st_apuc aggregates (change later once we figure out better agg method)
chardonnay_merg <- merge(Chardonnay, ag_st_apuc)
  colnames(chardonnay_merg)[8] <- c("gdd")
  
cabernet_merg <- merge(Cabernet, ag_st_apuc)
  colnames(cabernet_merg)[8] <- c("gdd")

rhone_merg <- merge(Rhone, ag_st_apuc)
  colnames(rhone_merg)[8] <- c("gdd")

merlot_merg <- merge(Merlot, ag_st_apuc)
  colnames(merlot_merg)[8] <- c("gdd")

zinfandel_merg <- merge(Zinfandel, ag_st_apuc)
  colnames(zinfandel_merg)[8] <- c("gdd")

#Plotting varieties along gdd
plot(R1_WS ~ gdd, 
     data = chardonnay_merg,
     cex = 1.2, #size?
     pch = 16, #fills circles?
     xlab = "Rank",
     ylab = "GDD",
     main = "Chardonnay",
     col = "purple")

plot(R1_WS ~ gdd, 
     data = cabernet_merg, 
     cex = 1.2, 
     pch = 16, 
     xlab = "Rank",
     ylab = "GDD",
     main = "Cabernet",
     col = "purple")

plot(R1_WS ~ gdd, 
     data = rhone_merg, 
     cex = 1.2, 
     pch = 16, 
     xlab = "Rank",
     ylab = "GDD",
     main = "Rhone",
     col = "purple")

plot(R1_WS ~ gdd, 
     data = merlot_merg, 
     cex = 1.2, 
     pch = 16, 
     xlab = "Rank",
     ylab = "GDD",
     main = "Merlot",
     col = "purple")

plot(R1_WS ~ gdd, 
     data = zinfandel_merg, 
     cex = 1.2, 
     pch = 16, 
     xlab = "Rank",
     ylab = "GDD",
     main = "Zinfandel",
     col = "purple")

