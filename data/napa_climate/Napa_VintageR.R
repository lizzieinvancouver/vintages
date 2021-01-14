################# Napa Vintage Dataset (PA) - Updated 12/2/2020 #########################

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
head(climdat_og)
zinfandelxy <- read.csv("/Users/phoebeautio/Desktop/Vintage Research/Zinfandelxy.csv", header=TRUE, na.strings=c(""," ","NA"))
head(mydat)

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
  
#TOBS
  plot(TAVG~TOBS, data = climdat, main = "TOBS vs. TAVG", xlim = c(-20, 40), ylim = c(-20, 40)) #positively correlated, tight near top and bottom but not in middle. One cluster. 

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

#correcting for NA PRCP values (replacing NAs with 0, for now)
  #st_hosp
  for(i in 1:nrow(st_hosp)){
    if(isTRUE(is.na(st_hosp[i, "PRCP"]))) {
      print(i)
      st_hosp$PRCP[i] <- 0
    }
  }
  
  #st_helena
  for(i in 1:nrow(st_helena)){
    if(isTRUE(is.na(st_helena[i, "PRCP"]))) {
      print(i)
      st_helena$PRCP[i] <- 0
    }
  }
  
  #st_apuc
  for(i in 1:nrow(st_apuc)){
    if(isTRUE(is.na(st_apuc[i, "PRCP"]))) {
      print(i)
      st_apuc$PRCP[i] <- 0
    }
  }
  
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
    st_hosp91.95 <- subset(st_hosp, st_hosp$year %in% c("1991", "1992", "1993", "1994", "1995")) 
    st_helena91.95 <- subset(st_helena, st_helena$year %in% c("1991", "1992", "1993", "1994", "1995"))
    st_apuc91.95 <- subset(st_apuc, st_apuc$year %in% c("1991", "1992", "1993", "1994", "1995"))

    st_hosp96.00 <- subset(st_hosp, st_hosp$year %in% c("1996", "1997", "1998", "1999", "2000"))
    st_helena96.00 <- subset(st_helena, st_helena$year %in% c("1996", "1997", "1998", "1999", "2000"))
    st_apuc96.00 <- subset(st_apuc, st_apuc$year %in% c("1996", "1997", "1998", "1999", "2000"))
    
    st_hosp01.05 <- subset(st_hosp, st_hosp$year %in% c("2001", "2002", "2003", "2004", "2005"))
    st_helena01.05 <- subset(st_helena, st_helena$year %in% c("2001", "2002", "2003", "2004", "2005"))
    st_apuc01.05 <- subset(st_apuc, st_apuc$year %in% c("2001", "2002", "2003", "2004", "2005"))
    
    st_hosp06.10 <- subset(st_hosp, st_hosp$year %in% c("2006", "2007", "2008", "2009", "2010"))
    st_helena06.10 <- subset(st_helena, st_helena$year %in% c("2006", "2007", "2008", "2009", "2010"))
    st_apuc06.10 <- subset(st_apuc, st_apuc$year %in% c("2006", "2007", "2008", "2009", "2010"))
 
    st_hosp11.15 <- subset(st_hosp, st_hosp$year %in% c("2011", "2012", "2013", "2014", "2015"))
    st_helena11.15 <- subset(st_helena, st_helena$year %in% c("2011", "2012", "2013", "2014", "2015"))
    st_apuc11.15 <- subset(st_apuc, st_apuc$year %in% c("2011", "2012", "2013", "2014", "2015"))
    
    #plotting
    
      #setting range
      range.x95 <- range(as.Date(st_hosp91.95$DATE, na.rm = TRUE))
      range.y95 <- range(st_hosp91.95$TAVG, na.rm = TRUE)
      
      range.x00 <- range(as.Date(st_hosp96.00$DATE, na.rm = TRUE))
      range.y00 <- range(st_hosp96.00$TAVG, na.rm = TRUE)
      
      range.x05 <- range(as.Date(st_hosp01.05$DATE, na.rm = TRUE))
      range.y05 <- range(st_hosp01.05$TAVG, na.rm = TRUE)
      
      range.x10 <- range(as.Date(st_hosp06.10$DATE, na.rm = TRUE))
      range.y10 <- range(st_hosp06.10$TAVG, na.rm = TRUE)
      
      range.x15 <- range(as.Date(st_hosp11.15$DATE, na.rm = TRUE))
      range.y15 <- range(st_hosp11.15$TAVG, na.rm = TRUE)
      
    plot(NA, xlim = range.x95, ylim = range.y95, xlab = "Date (1991-1995)", ylab = "Temperature ˚C", main = "TAVG 1991-1995", bty = "n")
    points(x = as.Date(st_hosp91.95$DATE), y = st_hosp91.95$TAVG, type = "l", col = "blue")
    points(x = as.Date(st_helena91.95$DATE), y = st_helena91.95$TAVG, type = "l", col = "green")
    points(x = as.Date(st_apuc91.95$DATE), y = st_apuc91.95$TAVG, type = "l", col = "orange")
    
    plot(NA, xlim = range.x00, ylim = range.y00, xlab = "Date (1996-2000)", ylab = "Temperature ˚C", main = "TAVG 1996-2000", bty = "n")
    points(x = as.Date(st_hosp96.00$DATE), y = st_hosp96.00$TAVG, type = "l", col = "blue")
    points(x = as.Date(st_helena96.00$DATE), y = st_helena96.00$TAVG, type = "l", col = "purple")
    points(x = as.Date(st_apuc96.00$DATE), y = st_apuc96.00$TAVG, type = "l", col = "orange")
    
    plot(NA, xlim = range.x05, ylim = range.y05, xlab = "Date (2001-2005)", ylab = "Temperature ˚C", main = "TAVG 2001-2005", bty = "n")
    points(x = as.Date(st_hosp01.05$DATE), y = st_hosp01.05$TAVG, type = "l", col = "blue")
    points(x = as.Date(st_helena01.05$DATE), y = st_helena01.05$TAVG, type = "l", col = "purple")
    points(x = as.Date(st_apuc01.05$DATE), y = st_apuc01.05$TAVG, type = "l", col = "orange")
    
    plot(NA, xlim = range.x10, ylim = range.y10, xlab = "Date (2006-2010)", ylab = "Temperature ˚C", main = "TAVG 2006-2010", bty = "n")
    points(x = as.Date(st_hosp06.10$DATE), y = st_hosp06.10$TAVG, type = "l", col = "blue")
    points(x = as.Date(st_helena06.10$DATE), y = st_helena06.10$TAVG, type = "l", col = "purple")
    points(x = as.Date(st_apuc06.10$DATE), y = st_apuc06.10$TAVG, type = "l", col = "orange")
    
    plot(NA, xlim = range.x15, ylim = range.y15, xlab = "Date (2011-2015)", ylab = "Temperature ˚C", main = "TAVG 2011-2015", bty = "n")
    points(x = as.Date(st_hosp11.15$DATE), y = st_hosp11.15$TAVG, type = "l", col = "blue")
    points(x = as.Date(st_helena11.15$DATE), y = st_helena11.15$TAVG, type = "l", col = "purple")
    points(x = as.Date(st_apuc11.15$DATE), y = st_apuc11.15$TAVG, type = "l", col = "orange")
    
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
climdat$prcpsum <- ave(climdat$PRCP, climdat$year, FUN=cumsum)
st_apuc$prcpsum <- ave(st_apuc$PRCP, st_apuc$year, FUN=cumsum)
st_helena$prcpsum <- ave(st_helena$PRCP, st_helena$year, FUN=cumsum)
st_hosp$prcpsum <- ave(st_hosp$PRCP, st_hosp$year, FUN=cumsum)

#creating dataframe of agg precipitation from each year and each location
agp_st_apuc <- aggregate(st_apuc$prcpsum, by = list(st_apuc$year), FUN=sum) 
colnames(agp_st_apuc)[1] <- c("Vintage")
colnames(agp_st_apuc)[2] <- c("st_apuc_prcp")

agp_st_helena <- aggregate(st_helena$prcpsum, by = list(st_helena$year), FUN=sum) 
colnames(agp_st_helena)[1] <- c("Vintage")
colnames(agp_st_helena)[2] <- c("st_helena_prcp")

agp_st_hosp <- aggregate(st_hosp$prcpsum, by = list(st_hosp$year), FUN=sum) 
colnames(agp_st_hosp)[1] <- c("Vintage")
colnames(agp_st_hosp)[2] <- c("st_hosp_prcp")

prcp_agg <- merge(agp_st_apuc, agp_st_helena)
prcp_agg <- merge(agp_st_hosp, prcp_agg)
prcp_agg$prcpsum <- rowMeans(prcp_agg[, 2:4])
colnames(prcp_agg)[5] <- c("prcp_avg")

### Vintage Data ###
#Subsetting by location (Napa/Sonoma)
  #Editing Rhône character
  mydat$Variety[which(mydat$Variety=="Rhône-Style Reds")] <- "Rhone"
  
napa <- mydat[which(mydat$Location=="Napa"), ]
  napa$Description_WS = NULL

sonoma <- mydat[which(mydat$Location=="Sonoma"), ]
  sonoma$Description_WS = NULL
  
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

#creating dataframea with gdd_agg, prcp_agg, vintage, and napa ratings -- edit this to one complete table
  Agg_Table <- merge(prcp_agg, gdd_agg)
  
  Cabernet_Table <- merge(Agg_Table, napa_cabernet)
  Chardonnay_Table <- merge(Agg_Table, napa_chardonnay)
  Merlot_Table <- merge(Agg_Table, napa_merlot)
  Rhone_Table <- merge(Agg_Table, napa_rhone)
  Zinfandel_Table <- merge(Agg_Table, napa_zinfandel)
  
  Final_Table_Full <- rbind(Cabernet_Table, Chardonnay_Table, Merlot_Table, Rhone_Table, Zinfandel_Table)
  Final_Table_Simp <- Final_Table_Full[-c(2,3,4,6,7,8)]
  
  #Exporting as csv for future modeling
   #write.csv(Final_Table_Full,"/Users/phoebeautio/Desktop/Vintage Research/VintageTableForModels.csv", row.names = FALSE)
   #write.csv(Final_Table_Simp,"/Users/phoebeautio/Desktop/Vintage Research/VintageTableForModelsSimp.csv", row.names = FALSE)
  
#Merging Varieties with the gdd_agg average (of 3 main stations) 
  #to plot total entries (sonoma and napa) run these lines and edit plots. Otherwise use the Variety napa tables.
  #Can likely delete later if napa data is complete enough.
  
  chardonnay_merg <- merge(Chardonnay, gdd_agg)
  colnames(chardonnay_merg)[8] <- c("gdd")
  
  cabernet_merg <- merge(Cabernet, gdd_agg)
  colnames(cabernet_merg)[8] <- c("gdd")
  
  rhone_merg <- merge(Rhone, gdd_agg)
  colnames(rhone_merg)[8] <- c("gdd")
  
  merlot_merg <- merge(Merlot, gdd_agg)
  colnames(merlot_merg)[8] <- c("gdd")
  
  zinfandel_merg <- merge(napa_zinfandel, gdd_agg)
  colnames(zinfandel_merg)[8] <- c("gdd")
  
#Plots 
#Chardonnay
  par(mfrow = c(2, 3))
  #Wine Spectator vs. Precipitation
  plot(R1_WS ~ prcp_avg, 
       data = Chardonnay_Table, 
       cex = 1.2, 
       pch = 16, 
       ylab = "Rank",
       xlab = "Average Precipitation",
       main = "Chardonnay Precipitation",
       col = "blue")
  
  #Wine Spectator vs. Avg. GDD Aggregate
    plot(R1_WS ~ gdd_avg, 
      data = Chardonnay_Table,
      cex = 1.2, #size
      pch = 16, #fills circles
      ylab = "Rank",
      xlab = "GDD",
      main = "Chardonnay GDD",
      col = "purple")

  #abline(lm(Chardonnay_Table$Chardonnay_WS ~ Chardonnay_Table$gdd_avg), col="black")
  chard_fittedmodel <- lm(Chardonnay_Table$R1_WS ~ Chardonnay_Table$gdd_avg * Chardonnay_Table$prcp_avg)
  summary(chard_fittedmodel)
  
#Cabernet
  #Wine Spectator vs. Precipitation
  plot(R1_WS ~ prcp_avg, 
       data = Cabernet_Table, 
       cex = 1.2, 
       pch = 16, 
       ylab = "Rank",
       xlab = "Average Precipitation",
       main = "Cabernet Precipitation",
       col = "blue")
  
  #Wine Spectator vs. Avg. GDD Aggregate
    plot(R1_WS ~ gdd_avg, 
      data = Cabernet_Table, 
      cex = 1.2, 
      pch = 16, 
      ylab = "Rank",
      xlab = "GDD",
      main = "Cabernet GDD",
      col = "purple")

  #abline(lm(Cabernet_Table$R1_WS ~ Cabernet_Table$gdd_avg), col="black")
  cab_fittedmodel <- lm(Cabernet_Table$R1_WS ~ Cabernet_Table$gdd_avg * Cabernet_Table$prcp_avg)
  summary(cab_fittedmodel)

#Rhone
  #Wine Spectator vs. Precipitation
    plot(R1_WS ~ prcp_avg, 
      data = Rhone_Table, 
      cex = 1.2, 
      pch = 16, 
      ylab = "Rank",
      xlab = "Average Precipitation",
      main = "Rhone Precipitation",
      col = "blue")

  #Wine Spectator vs. Avg. GDD Aggregate
    plot(R1_WS ~ gdd_avg, 
     data = Rhone_Table, 
     cex = 1.2, 
     pch = 16, 
     ylab = "Rank",
     xlab = "GDD",
     main = "Rhone GDD",
     col = "purple")

#abline(lm(Rhone_Table$R1_WS ~ Rhone_Table$gdd_avg), col="black")
rhone_fittedmodel <- lm(Rhone_Table$R1_WS ~ Rhone_Table$gdd_avg * Rhone_Table$prcp_avg)
summary(rhone_fittedmodel)

#Merlot
  par(mfrow = c(2, 3))
  #Wine Spectator vs. Precipitation
    plot(R1_WS ~ prcp_avg, 
      data = Merlot_Table, 
      cex = 1.2, 
      pch = 16, 
      ylab = "Rank",
      xlab = "Average Precipitation",
      main = "Merlot Precipitation",
      col = "blue")

  #Wine Spectator vs. Avg. GDD Aggregate
    plot(R1_WS ~ gdd_avg, 
      data = Merlot_Table, 
      cex = 1.2, 
      pch = 16, 
      ylab = "Rank",
      xlab = "GDD",
      main = "Merlot GDD",
      col = "purple")

#abline(lm(Merlot_Table$R1_WS ~ Merlot_Table$gdd_avg), col="black")
mer_fittedmodel <- lm(Merlot_Table$R1_WS ~ Merlot_Table$gdd_avg * Merlot_Table$prcp_avg)
summary(mer_fittedmodel)

#Zinfandel
  #Wine Spectator vs. Avg. GDD Aggregate
     plot(R1_WS ~ gdd_avg, 
      data = Zinfandel_Table, 
      cex = 1.2, 
      pch = 16, 
      ylab = "Rank",
      xlab = "GDD",
      main = "Zinfandel_WS GDD",
      col = "purple")

  #Wine Enthusiast vs. Avg. GDD Aggregate
    plot(R2_WE ~ gdd_avg, 
      data = Zinfandel_Table, 
      cex = 1.2, 
      pch = 16, 
      ylab = "Rank",
      xlab = "GDD",
      main = "Zinfandel_WE GDD",
      col = "purple")

  #Wine Spectator & Wine Enthusiast Rankings vs. GDD Aggregate
    range.x <- range(Zinfandel_Table$gdd_avg)
    plot(NA, 
         xlim = range.x, 
         ylim = c(80, 100), 
         ylab = "Rank (b = WS, o = WE)", 
         xlab = "GDD",
         main = "Zinfandel_WS+WE GDD",
         bty = "n")
    
    points(x = Zinfandel_Table$gdd_avg, 
           y = Zinfandel_Table$R1_WS, 
           type = "p", 
           pch = 16, 
           col = "blue")
    
    points(x = Zinfandel_Table$gdd_avg, 
           y = Zinfandel_Table$R2_WE, 
           type = "p", 
           pch = 16, 
           col = "orange")
  
  #Wine Spectator & Wine Enthusiast Rankings vs. Precipitation 
    range.zinx <- range(Zinfandel_Table$prcp_avg)
    plot(NA, 
         xlim = range.zinx, 
         ylim = c(80, 100), 
         ylab = "Rank (b = WS, o = WE)", 
         xlab = "Averge Precipitation",
         main = "Zinfandel_WS+WE Precip.",
         bty = "n")
    
    points(x = Zinfandel_Table$prcp_avg, 
           y = Zinfandel_Table$R1_WS, 
           type = "p", 
           pch = 16, 
           col = "blue")
    
    points(x = Zinfandel_Table$prcp_avg, 
           y = Zinfandel_Table$R2_WE, 
           type = "p", 
           pch = 16, 
           col = "orange")
    
  #Wine Spectator & Wine Enthusiast correlation (By Year)
    plot(NA, 
         xlim = c(2004, 2018),
         ylim = c(80, 100), 
         ylab = "Rank (b = WS, o = WE)", 
         xlab = "GDD",
         main = "Zinfandel WS vs. WE by Year",
         bty = "n")
    
    points(x = Zinfandel_Table$Vintage, 
           y = Zinfandel_Table$R1_WS, 
           type = "p", 
           pch = 16, 
           col = "blue")
    
    points(x = Zinfandel_Table$Vintage, 
           y = Zinfandel_Table$R2_WE, 
           type = "p", 
           pch = 16, 
           col = "orange")
    
  #Wine Spectator & Wine Enthusiast correlation (XY)
    range.zinx <- range(Zinfandel_Table$R1_WE, na.rm = TRUE)
    range.ziny <- range(Zinfandel_Table$R1_WS, na.rm = TRUE)
    
    plot(zinfandelxy$Zinfandel_WS ~ zinfandelxy$Zinfandel_WE, 
         ylab = "WS Rank",
         xlab = "WE Rank",
         main = "Zinfandel WS vs. WE",
         col = "purple") 
    
#abline(lm(Zinfandel_Table$R1_WS ~ Zinfandel_Tableg$gdd_avg), col="black")
#abline(lm(Zinfandel_Table$Zinfandel_WE ~ Zinfandel_Tableg$gdd_avg), col="black")
zin_fittedmodel <- lm(Zinfandel_Table$R1_WS ~ Zinfandel_Table$gdd_avg * Zinfandel_Table$prcp_avg)
summary(zin_fittedmodel)

----notes--------------------
  fittedmodel <- lm(ranking ~ gdd_avg * prep_avg)
  
  #pdf(file = "Variety_vs_GDD.pdf", width = 6, height = 6)
  
  lm(ranking ~ gdd_avg + prep_avg + gdd_avg * prep_avg)
  summary(fittedmodel)

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

#-----double y axis plots-----
#Precipitation/Rank, Dual y-axis
  #time along x, GDD/temp and Precipitation on the Y. Difficult to put in rank, if we wanted to go crazy we would do a 3D plot, z axis as rank (predicted)
  #Merlot
  par(mar = c(5, 5, 3, 5))
plot(R1_WS ~ Vintage, 
     data = Merlot_Table, 
     cex = 1.2, 
     pch = 16, 
     ylab = "Rank",
     xlab = "GDD",
     main = "Merlot",
     col = "purple")
par(new = TRUE)

plot(Merlot_Table$prcp_avg, type = "l", xaxt = "n", yaxt = "n",
     ylab = "", xlab = "", col = "red", lty = 2)
axis(side = 4)
mtext("Average Precipitation", side = 4, line = 3)

#legend("topleft", c("Rank", "Average Precipitation"),
#col = c("Purple", "red"), lty = c(1, 2))
