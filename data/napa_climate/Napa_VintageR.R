################# Napa Vintage Dataset (PA) - Updated 11/14/2020 #########################

#Housekeeping
rm(list=ls())
options(stringsAsFactors = FALSE)

#Load libraries
library(lubridate)
library(dplyr)

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
climdat$TAVG <- rowMeans(climdat[c('TMAX', 'TMIN')], na.rm=FALSE)
par(mfrow = c(2, 3))
plot(TAVG~TOBS, 
     data = climdat, 
     xlim = c(-20, 40), 
     ylim = c(-20, 40)) #positively correlated, tight near top and bottom but not in middle. One cluster. 

#Subset by location and year to test weather station trajectories
#Subsetting locations
unique(climdat$NAME)
unique(climdat$STATION)

st_hosp <- subset(climdat, climdat$STATION=="USC00046074") #NAPA STATE HOSPITAL***
st_helena <- subset(climdat, climdat$STATION=="USC00047643") #SAINT HELENA***
st_apuc <- subset(climdat, climdat$STATION=="USC00040212") #ANGWIN PACIFIC UNION COLLEGE***

#correcting for NA TAVG values (averaging the day prior to and after the missing date)
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

  st_helena$TAVG[which(st_helena$DATE=="1993-04-29" | st_helena$DATE=="1993-04-30")] <- 20.58 #cleaning NAs
  st_helena$TAVG[which(st_helena$DATE=="2002-09-29" | st_helena$DATE=="2002-09-30")] <- 15.83 #cleaning NAs
  st_helena$TAVG[which(st_helena$DATE=="2004-06-18" | st_helena$DATE=="2004-06-19" | 
                         st_helena$DATE=="2004-06-20" | st_helena$DATE=="2004-06-21" | 
                         st_helena$DATE=="2004-06-22" | st_helena$DATE=="2004-06-23")] <- 24.45 #cleaning NAs
  st_helena$TAVG[which(st_helena$DATE=="2004-09-12" | st_helena$DATE=="2004-09-13")] <- 23.60 #cleaning NAs
  st_helena$TAVG[which(st_helena$DATE=="2004-09-29" | st_helena$DATE=="2004-09-30")] <- 15.55 #cleaning NAs
  st_helena$TAVG[which(st_helena$DATE=="2005-09-28" | st_helena$DATE=="2005-09-29" | st_helena$DATE=="2005-09-30")] <- 16.25 #cleaning NAs
  st_helena$TAVG[which(st_helena$DATE=="2006-06-13" | st_helena$DATE=="2006-06-14")] <- 18.48 #cleaning NAs
  st_helena$TAVG[which(st_helena$DATE=="2008-09-18" | st_helena$DATE=="2008-09-19")] <- 16.93 #cleaning NAs
  st_helena$TAVG[which(st_helena$DATE=="2009-07-09" | st_helena$DATE=="2009-07-10" | st_helena$DATE=="2009-07-11")] <- 20.40 #cleaning NAs
  
  #st_apuc
  for(i in 1:nrow(st_apuc)){
    if(isTRUE(is.na(st_apuc[i, "TAVG"]))) {
      print(i)
      st_apuc$TAVG[i] <- sum(st_apuc$TAVG[i-1], st_apuc$TAVG[i+1]) / 2
    }
  }
  
  st_apuc$TAVG[which(st_apuc$DATE=="2007-04-13" | st_apuc$DATE=="2007-04-14")] <- 10.55 #cleaning NAs

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
  #subsetting 5 years of data
    st_hosp91.95 <- subset(st_hosp, st_hosp$year=="1991" | st_hosp$year=="1992" | st_hosp$year=="1993" | st_hosp$year=="1994" | st_hosp$year=="1995") 
    st_helena91.95 <- subset(st_helena, st_helena$year=="1991" | st_helena$year=="1992" | st_helena$year=="1993" | st_helena$year=="1994" | st_helena$year=="1995")
    st_apuc91.95 <- subset(st_apuc, st_apuc$year=="1991" | st_apuc$year=="1992" | st_apuc$year=="1993" | st_apuc$year=="1994" | st_apuc$year=="1995")

    st_hosp96.00 <- subset(st_hosp, st_hosp$year=="1996" | st_hosp$year=="1997" | st_hosp$year=="1998" | st_hosp$year=="1999" | st_hosp$year=="2000") 
    st_helena96.00 <- subset(st_helena, st_helena$year=="1996" | st_helena$year=="1997" | st_helena$year=="1998" | st_helena$year=="1999" | st_helena$year=="2000")
    st_apuc96.00 <- subset(st_apuc, st_apuc$year=="1996" | st_apuc$year=="1997" | st_apuc$year=="1998" | st_apuc$year=="1999" | st_apuc$year=="2000")
    
    st_hosp01.05 <- subset(st_hosp, st_hosp$year=="2001" | st_hosp$year=="2002" | st_hosp$year=="2003" | st_hosp$year=="2004" | st_hosp$year=="2005") 
    st_helena01.05 <- subset(st_helena, st_helena$year=="2001" | st_helena$year=="2002" | st_helena$year=="2003" | st_helena$year=="2004" | st_helena$year=="2005")
    st_apuc01.05 <- subset(st_apuc, st_apuc$year=="2001" | st_apuc$year=="2002" | st_apuc$year=="2003" | st_apuc$year=="2004" | st_apuc$year=="2005")
    
    st_hosp06.10 <- subset(st_hosp, st_hosp$year=="2006" | st_hosp$year=="2007" | st_hosp$year=="2008" | st_hosp$year=="2009" | st_hosp$year=="2010") 
    st_helena06.10 <- subset(st_helena, st_helena$year=="2006" | st_helena$year=="2007" | st_helena$year=="2008" | st_helena$year=="2009" | st_helena$year=="2010")
    st_apuc06.10 <- subset(st_apuc, st_apuc$year=="2006" | st_apuc$year=="2007" | st_apuc$year=="2008" | st_apuc$year=="2009" | st_apuc$year=="2010")
    
    st_hosp11.15 <- subset(st_hosp, st_hosp$year=="2011" | st_hosp$year=="2012" | st_hosp$year=="2013" | st_hosp$year=="2014" | st_hosp$year=="2015") 
    st_helena11.15 <- subset(st_helena, st_helena$year=="2011" | st_helena$year=="2012" | st_helena$year=="2013" | st_helena$year=="2014" | st_helena$year=="2015")
    st_apuc11.15 <- subset(st_apuc, st_apuc$year=="2011" | st_apuc$year=="2012" | st_apuc$year=="2013" | st_apuc$year=="2014" | st_apuc$year=="2015")
    
    #plotting
      #setting range
      range.x95 <- range(st_hosp91.95$year, na.rm = TRUE)
      range.y95 <- range(st_hosp91.95$TAVG, na.rm = TRUE)
      
      range.x00 <- range(st_hosp96.00$year, na.rm = TRUE)
      range.y00 <- range(st_hosp96.00$TAVG, na.rm = TRUE)
      
      range.x05 <- range(st_hosp01.05$year, na.rm = TRUE)
      range.y05 <- range(st_hosp01.05$TAVG, na.rm = TRUE)
      
      range.x10 <- range(st_hosp06.10$year, na.rm = TRUE)
      range.y10 <- range(st_hosp06.10$TAVG, na.rm = TRUE)
      
      range.x15 <- range(st_hosp11.15$year, na.rm = TRUE)
      range.y15 <- range(st_hosp11.15$TAVG, na.rm = TRUE)
      
    plot(NA, xlim = range.x95, ylim = range.y95, xlab = "Date (1991-1995)", ylab = "Temperature ˚C", main = "TAVG Tragectories 1991-1995", bty = "n")
    points(x = st_hosp91.95$year, y = st_hosp91.95$TAVG, type = "l", col = "blue")
    points(x = st_helena91.95$year, y = st_helena91.95$TAVG, type = "l", col = "purple")
    points(x = st_apuc91.95$year, y = st_apuc91.95$TAVG, type = "l", col = "orange")
    
    plot(NA, xlim = range.x00, ylim = range.y00, xlab = "Date (1996-2000)", ylab = "Temperature ˚C", main = "TAVG Tragectories 1996-2000", bty = "n")
    points(x = st_hosp96.00$year, y = st_hosp96.00$TAVG, type = "l", col = "blue")
    points(x = st_helena96.00$year, y = st_helena96.00$TAVG, type = "l", col = "purple")
    points(x = st_apuc96.00$year, y = st_apuc96.00$TAVG, type = "l", col = "orange")
    
    plot(NA, xlim = range.x05, ylim = range.y05, xlab = "Date (2001-2005)", ylab = "Temperature ˚C", main = "TAVG Tragectories 2001-2005", bty = "n")
    points(x = st_hosp01.05$year, y = st_hosp01.05$TAVG, type = "l", col = "blue")
    points(x = st_helena01.05$year, y = st_helena01.05$TAVG, type = "l", col = "purple")
    points(x = st_apuc01.05$year, y = st_apuc01.05$TAVG, type = "l", col = "orange")
    
    plot(NA, xlim = range.x10, ylim = range.y10, xlab = "Date (2006-2010)", ylab = "Temperature ˚C", main = "TAVG Tragectories 2006-2010", bty = "n")
    points(x = st_hosp06.10$year, y = st_hosp06.10$TAVG, type = "l", col = "blue")
    points(x = st_helena06.10$year, y = st_helena06.10$TAVG, type = "l", col = "purple")
    points(x = st_apuc06.10$year, y = st_apuc06.10$TAVG, type = "l", col = "orange")
    
    plot(NA, xlim = range.x15, ylim = range.y15, xlab = "Date (2011-2015)", ylab = "Temperature ˚C", main = "TAVG Tragectories 2011-2015", bty = "n")
    points(x = st_hosp11.15$year, y = st_hosp11.15$TAVG, type = "l", col = "blue")
    points(x = st_helena11.15$year, y = st_helena11.15$TAVG, type = "l", col = "purple")
    points(x = st_apuc11.15$year, y = st_apuc11.15$TAVG, type = "l", col = "orange")
    
#Calculating GDD at each location -- GDD base temp = 10
climdat$gddbase <- ifelse(climdat$TAVG >= 10, climdat$TAVG - 10, 0)
climdat$gdd <- ave(climdat$gddbase, climdat$year, FUN=cumsum)

st_apuc$gddbase <- ifelse(st_apuc$TAVG >= 10, st_apuc$TAVG - 10, 0)
st_apuc$gdd <- ave(st_apuc$gddbase, st_apuc$year, FUN=cumsum)

st_helena$gddbase <- ifelse(st_helena$TAVG >= 10, st_helena$TAVG - 10, 0)
st_helena$gdd <- ave(st_helena$gddbase, st_helena$year, FUN=cumsum)

st_hosp$gddbase <- ifelse(st_hosp$TAVG >= 10, st_hosp$TAVG - 10, 0)
st_hosp$gdd <- ave(st_hosp$gddbase, st_hosp$year, FUN=cumsum)

#creating dataframe of agg gdd from each year and each location
ag_st_apuc <- aggregate(st_apuc$gddbase, by = list(st_apuc$year), FUN=sum) 
colnames(ag_st_apuc)[1] <- c("Vintage")
colnames(ag_st_apuc)[2] <- c("st_apuc_gdd")

ag_st_helena <- aggregate(st_helena$gddbase, by = list(st_helena$year), FUN=sum) 
colnames(ag_st_helena)[1] <- c("Vintage")
colnames(ag_st_helena)[2] <- c("st_helena_gdd")

ag_st_hosp <- aggregate(st_hosp$gddbase, by = list(st_hosp$year), FUN=sum) 
colnames(ag_st_hosp)[1] <- c("Vintage")
colnames(ag_st_hosp)[2] <- c("st_hosp_gdd")

gdd_agg <- merge(ag_st_apuc, ag_st_helena)
gdd_agg <- merge(ag_st_hosp, gdd_agg)
gdd_agg$gdd_avg <- rowMeans(gdd_agg[, 2:4])
  
#Calculating Precipitation at each location
  #how to plot along with TAVG
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
mydat$Variety[which(mydat$Variety=="Rhône-Style Reds")] <- "Rhone"
  
#Subsetting by Variety and location
unique(mydat$Variety)

  #variety
  Cabernet <- subset(mydat, mydat$Variety=="Cabernet")
  Chardonnay <- subset(mydat, mydat$Variety=="Chardonnay")
  Rhone <- subset(mydat, mydat$Variety=="Rhone")
  Merlot <- subset(mydat, mydat$Variety=="Merlot")
  Zinfandel <- subset(mydat, mydat$Variety=="Zinfandel")

  #location
  napa_cabernet <- subset(napa, napa$Variety=="Cabernet")
  napa_chardonnay <- subset(napa, napa$Variety=="Chardonnay")
  napa_rhone <- subset(napa, napa$Variety=="Rhone")
  napa_merlot <- subset(napa, napa$Variety=="Merlot")
  napa_zinfandel <- subset(napa, napa$Variety=="Zinfandel")

  #location
  sonoma_cabernet <- subset(sonoma, sonoma$Variety=="Cabernet")
  sonoma_chardonnay <- subset(sonoma, sonoma$Variety=="Chardonnay")
  sonoma_rhone <- subset(sonoma, sonoma$Variety=="Rhone")
  sonoma_merlot <- subset(sonoma, sonoma$Variety=="Merlot")
  sonoma_zinfandel <- subset(sonoma, sonoma$Variety=="Zinfandel")

#Merging Varieties with the gdd_agg average (of 3 main stations) 
  #to plot total entries (sonoma and napa) run lines 254-272. to plot just napa, run 274-292.
  #sonome/napa
  chardonnay_merg <- merge(Chardonnay, gdd_agg)
  colnames(chardonnay_merg)[8] <- c("gdd")
  
  #sonome/napa
  cabernet_merg <- merge(Cabernet, gdd_agg)
  colnames(cabernet_merg)[8] <- c("gdd")
  
  #sonome/napa
  rhone_merg <- merge(Rhone, gdd_agg)
  colnames(rhone_merg)[8] <- c("gdd")
  
  #sonome/napa
  merlot_merg <- merge(Merlot, gdd_agg)
  colnames(merlot_merg)[8] <- c("gdd")
  
  #sonome/napa
  zinfandel_merg <- merge(napa_zinfandel, gdd_agg)
  colnames(zinfandel_merg)[8] <- c("gdd")
  
  #napa
  chardonnay_merg <- merge(napa_chardonnay, gdd_agg)
  colnames(chardonnay_merg)[8] <- c("gdd")
  
  #napa
  cabernet_merg <- merge(napa_cabernet, gdd_agg)
  colnames(cabernet_merg)[8] <- c("gdd")
  
  #napa - not running correctly
  rhone_merg <- merge(napa_rhone, gdd_agg)
  colnames(rhone_merg)[8] <- c("gdd")
  
  #napa
  merlot_merg <- merge(napa_merlot, gdd_agg)
  colnames(merlot_merg)[8] <- c("gdd")

  #napa
  zinfandel_merg <- merge(Zinfandel, gdd_agg)
  colnames(zinfandel_merg)[8] <- c("gdd")

#Plotting varieties along gdd
#pdf(file = "Variety_vs_GDD.pdf", width = 6, height = 6)
#par(mfrow = c(2, 3))

#Chardonnay
plot(R1_WS ~ gdd_avg, 
     data = chardonnay_merg,
     cex = 1.2, #size
     pch = 16, #fills circles
     ylab = "Rank",
     xlab = "GDD",
     main = "Chardonnay",
     col = "purple")

abline(lm(chardonnay_merg$R1_WS ~ chardonnay_merg$gdd_avg), col="black")

#Cabernet
plot(R1_WS ~ gdd_avg, 
     data = cabernet_merg, 
     cex = 1.2, 
     pch = 16, 
     ylab = "Rank",
     xlab = "GDD",
     main = "Cabernet",
     col = "purple")

abline(lm(cabernet_merg$R1_WS ~ cabernet_merg$gdd_avg), col="black")

#Rhone
plot(R1_WS ~ gdd_avg, 
     data = rhone_merg, 
     cex = 1.2, 
     pch = 16, 
     ylab = "Rank",
     xlab = "GDD",
     main = "Rhone",
     col = "purple")

abline(lm(rhone_merg$R1_WS ~ rhone_merg$gdd_avg), col="black")

#Merlot
plot(R1_WS ~ gdd_avg, 
     data = merlot_merg, 
     cex = 1.2, 
     pch = 16, 
     ylab = "Rank",
     xlab = "GDD",
     main = "Merlot",
     col = "purple")

abline(lm(merlot_merg$R1_WS ~ merlot_merg$gdd_avg), col="black")

#Zinfandel
plot(R1_WS ~ gdd_avg, 
     data = zinfandel_merg, 
     cex = 1.2, 
     pch = 16, 
     ylab = "Rank",
     xlab = "GDD",
     main = "Zinfandel",
     col = "purple")

abline(lm(zinfandel_merg$R1_WS ~ zinfandel_merg$gdd_avg), col="black")

----notes--------------------
plot(NA, 
     xlim = c(0, 10), 
     ylim = c(0, 10), 
     xlab = "Date", 
     ylab = "Temperature", 
     bty = "n")

points(x = ..., 
       y = ..., 
       type = "l", 
       col = "blue")

#Specifying range before plotting
range(st_helena$DATE, na.rm = TRUE)
range.x <- range(st_helena$DATE, na.rm = TRUE)
  range.x <- as.Date(range.x)
range.y <- range(st_helena$TAVG, na.rm = TRUE)

plot(NA, 
     xlim = range.x, 
     ylim = range.y, 
     xlab = "Date", 
     ylab = "Temperature", 
     bty = "n")

points(st_helena$TAVG ~ as.Date(st_helena$DATE), 
       type = "l", 
       col = "blue")

points(st_apuc$TAVG ~ as.Date(st_apuc$DATE), 
       type = "l", 
       col = "green")

points(y ~ x, type = "p", col = "red")


-----
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

