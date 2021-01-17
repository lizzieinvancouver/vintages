################# Napa Vintage Dataset (PA) - Updated 1/17/2021 #########################

#Housekeeping
rm(list=ls())
options(stringsAsFactors = FALSE)

#Load libraries
library(lubridate)
library(dplyr)

#Reading in csv files
mydat <- read.csv("/Users/phoebeautio/Desktop/Vintage Research/Napa_Vintage.csv", header=TRUE, na.strings=c(""," ","NA"))
head(mydat)

st_helena <- read.csv("/Users/phoebeautio/Desktop/Vintage Research/StHelenaClean.csv", header=TRUE, na.strings=c(""," ","NA"))
head(st_helena)

st_hosp <- read.csv("/Users/phoebeautio/Desktop/Vintage Research/StHospClean.csv", header=TRUE, na.strings=c(""," ","NA"))
head(st_hosp)

climdat_og <- read.csv("/Users/phoebeautio/Desktop/Vintage Research/Napa_1990-2019.csv", header=TRUE, na.strings=c(""," ","NA"))
head(climdat_og)

zinfandelxy <- read.csv("/Users/phoebeautio/Desktop/Vintage Research/Zinfandelxy.csv", header=TRUE, na.strings=c(""," ","NA"))

## Relative paths (Geoff)
## mydat <- read.csv("Napa_Vintage.csv", header=TRUE, na.strings=c(""," ","NA"))
## head(mydat)
## climdat_og <- read.csv("Napa_1990-2019.csv", header=TRUE, na.strings=c(""," ","NA"))

#Plotting datasets to view tragectory
  #subsetting 5 years of data
    st_hosp91.95 <- subset(st_hosp, st_hosp$year %in% c("1991", "1992", "1993", "1994", "1995")) 
    st_helena91.95 <- subset(st_helena, st_helena$year %in% c("1991", "1992", "1993", "1994", "1995"))

    st_hosp96.00 <- subset(st_hosp, st_hosp$year %in% c("1996", "1997", "1998", "1999", "2000"))
    st_helena96.00 <- subset(st_helena, st_helena$year %in% c("1996", "1997", "1998", "1999", "2000"))
    
    st_hosp01.05 <- subset(st_hosp, st_hosp$year %in% c("2001", "2002", "2003", "2004", "2005"))
    st_helena01.05 <- subset(st_helena, st_helena$year %in% c("2001", "2002", "2003", "2004", "2005"))
    
    st_hosp06.10 <- subset(st_hosp, st_hosp$year %in% c("2006", "2007", "2008", "2009", "2010"))
    st_helena06.10 <- subset(st_helena, st_helena$year %in% c("2006", "2007", "2008", "2009", "2010"))
 
    st_hosp11.15 <- subset(st_hosp, st_hosp$year %in% c("2011", "2012", "2013", "2014", "2015"))
    st_helena11.15 <- subset(st_helena, st_helena$year %in% c("2011", "2012", "2013", "2014", "2015"))
    
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
    
    plot(NA, xlim = range.x00, ylim = range.y00, xlab = "Date (1996-2000)", ylab = "Temperature ˚C", main = "TAVG 1996-2000", bty = "n")
    points(x = as.Date(st_hosp96.00$DATE), y = st_hosp96.00$TAVG, type = "l", col = "blue")
    points(x = as.Date(st_helena96.00$DATE), y = st_helena96.00$TAVG, type = "l", col = "purple")
    
    plot(NA, xlim = range.x05, ylim = range.y05, xlab = "Date (2001-2005)", ylab = "Temperature ˚C", main = "TAVG 2001-2005", bty = "n")
    points(x = as.Date(st_hosp01.05$DATE), y = st_hosp01.05$TAVG, type = "l", col = "blue")
    points(x = as.Date(st_helena01.05$DATE), y = st_helena01.05$TAVG, type = "l", col = "purple")
    
    plot(NA, xlim = range.x10, ylim = range.y10, xlab = "Date (2006-2010)", ylab = "Temperature ˚C", main = "TAVG 2006-2010", bty = "n")
    points(x = as.Date(st_hosp06.10$DATE), y = st_hosp06.10$TAVG, type = "l", col = "blue")
    points(x = as.Date(st_helena06.10$DATE), y = st_helena06.10$TAVG, type = "l", col = "purple")
    
    plot(NA, xlim = range.x15, ylim = range.y15, xlab = "Date (2011-2015)", ylab = "Temperature ˚C", main = "TAVG 2011-2015", bty = "n")
    points(x = as.Date(st_hosp11.15$DATE), y = st_hosp11.15$TAVG, type = "l", col = "blue")
    points(x = as.Date(st_helena11.15$DATE), y = st_helena11.15$TAVG, type = "l", col = "purple")
    
#Calculating GDD at each location -- GDD base temp = 10
st_helena$gddbase <- ifelse(st_helena$TAVG >= 10, st_helena$TAVG - 10, 0)
st_helena$gdd <- ave(st_helena$gddbase, st_helena$year, FUN=cumsum)

st_hosp$gddbase <- ifelse(st_hosp$TAVG >= 10, st_hosp$TAVG - 10, 0)
st_hosp$gdd <- ave(st_hosp$gddbase, st_hosp$year, FUN=cumsum)

#creating dataframe of agg gdd from each year and each location
ag_st_helena <- aggregate(st_helena$gddbase, by = list(st_helena$year), FUN=sum) 
colnames(ag_st_helena)[1] <- c("Vintage")
colnames(ag_st_helena)[2] <- c("st_helena_gdd")

ag_st_hosp <- aggregate(st_hosp$gddbase, by = list(st_hosp$year), FUN=sum) 
colnames(ag_st_hosp)[1] <- c("Vintage")
colnames(ag_st_hosp)[2] <- c("st_hosp_gdd")

gdd_agg <- merge(ag_st_helena, ag_st_hosp)
gdd_agg$gdd_avg <- rowMeans(gdd_agg[, 2:3])


#Calculating GDD at each location by phenology
#Calculating precipitation at each location by phenology

  
#Calculating Precipitation at each location
st_helena$prcpsum <- ave(st_helena$PRCP, st_helena$year, FUN=cumsum)
st_hosp$prcpsum <- ave(st_hosp$PRCP, st_hosp$year, FUN=cumsum)

#creating dataframe of agg precipitation from each year and each location
agp_st_helena <- aggregate(st_helena$prcpsum, by = list(st_helena$year), FUN=sum) 
colnames(agp_st_helena)[1] <- c("Vintage")
colnames(agp_st_helena)[2] <- c("st_helena_prcp")

agp_st_hosp <- aggregate(st_hosp$prcpsum, by = list(st_hosp$year), FUN=sum) 
colnames(agp_st_hosp)[1] <- c("Vintage")
colnames(agp_st_hosp)[2] <- c("st_hosp_prcp")

prcp_agg <- merge(agp_st_helena, agp_st_hosp)
prcp_agg$prcpsum <- rowMeans(prcp_agg[, 2:3])
colnames(prcp_agg)[4] <- c("prcp_avg")

#Subsetting by location (Napa/Sonoma)
  #Editing Rhône character
  mydat$Variety[which(mydat$Variety=="Rhône-Style Reds")] <- "Rhone"
  
napa <- mydat[which(mydat$Location=="Napa"), ]
  napa$Description_WS = NULL

sonoma <- mydat[which(mydat$Location=="Sonoma"), ]
  sonoma$Description_WS = NULL
  
north_coast <- mydat[which(mydat$Location=="North Coast"), ]
  north_coast$Description_WS = NULL
  
#Subsetting by Variety and location
unique(mydat$Variety)

  #variety
  Cabernet <- subset(mydat, mydat$Variety=="Cabernet")
  Chardonnay <- subset(mydat, mydat$Variety=="Chardonnay")
  Rhone <- subset(mydat, mydat$Variety=="Rhone")
  Merlot <- subset(mydat, mydat$Variety=="Merlot")
  Zinfandel <- subset(mydat, mydat$Variety=="Zinfandel")

  #location - napa
  napa_cabernet <- subset(napa, napa$Variety=="Cabernet")
  napa_chardonnay <- subset(napa, napa$Variety=="Chardonnay")
  napa_rhone <- subset(napa, napa$Variety=="Rhone")
  napa_merlot <- subset(napa, napa$Variety=="Merlot")
  napa_zinfandel <- subset(napa, napa$Variety=="Zinfandel")

  #location - sonoma
  sonoma_cabernet <- subset(sonoma, sonoma$Variety=="Cabernet")
  sonoma_chardonnay <- subset(sonoma, sonoma$Variety=="Chardonnay")
  sonoma_rhone <- subset(sonoma, sonoma$Variety=="Rhone")
  sonoma_merlot <- subset(sonoma, sonoma$Variety=="Merlot")
  sonoma_zinfandel <- subset(sonoma, sonoma$Variety=="Zinfandel")
  
  #location - north coast
  nc_cabernet <- subset(north_coast, sonoma$Variety=="Cabernet")
  nc_chardonnay <- subset(north_coast, sonoma$Variety=="Chardonnay")
  nc_zinfandel <- subset(north_coast, sonoma$Variety=="Zinfandel")

#creating dataframe with gdd_agg, prcp_agg, vintage, and napa ratings
  Agg_Table <- merge(prcp_agg, gdd_agg)
  
  #napa
  Cabernet_Table_N <- merge(Agg_Table, napa_cabernet)
  Chardonnay_Table_N <- merge(Agg_Table, napa_chardonnay)
  Merlot_Table_N <- merge(Agg_Table, napa_merlot)
  Rhone_Table_N <- merge(Agg_Table, napa_rhone)
  Zinfandel_Table_N <- merge(Agg_Table, napa_zinfandel)
  
  Napa_Table_Full <- rbind(Cabernet_Table_N, Chardonnay_Table_N, Merlot_Table_N, Rhone_Table_N, Zinfandel_Table_N)
  
  #sonoma
  Cabernet_Table_S <- merge(Agg_Table, sonoma_cabernet)
  Chardonnay_Table_S <- merge(Agg_Table, sonoma_chardonnay)
  Merlot_Table_S <- merge(Agg_Table, sonoma_merlot)
  Rhone_Table_S <- merge(Agg_Table, sonoma_rhone)
  Zinfandel_Table_S <- merge(Agg_Table, sonoma_zinfandel)
  
  Sonoma_Table_Full <- rbind(Cabernet_Table_S, Chardonnay_Table_S, Merlot_Table_S, Rhone_Table_S, Zinfandel_Table_S)
  
  #north coast
  Cabernet_Table_NC <- merge(Agg_Table, nc_cabernet)
  Chardonnay_Table_NC <- merge(Agg_Table, nc_chardonnay)
  Zinfandel_Table_NC <- merge(Agg_Table, nc_zinfandel)
  
  NC_Table_Full <- rbind(Cabernet_Table_NC, Chardonnay_Table_NC, Zinfandel_Table_NC)
  
  #Exporting as csv for future modeling
   #write.csv(Napa_Table_Full,"/Users/phoebeautio/Desktop/Vintage Research/NapaComplete.csv", row.names = FALSE)
   #write.csv(Sonoma_Table_Full,"/Users/phoebeautio/Desktop/Vintage Research/SonomaComplete.csv", row.names = FALSE)
   #write.csv(NC_Table_Full,"/Users/phoebeautio/Desktop/Vintage Research/NorthCoastComplete.csv", row.names = FALSE)

  
  
  #------------ need to update naming in this section, will replace with more efficient loops
  
#Merging Varieties with the gdd_agg average (of 2 main stations) 
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

