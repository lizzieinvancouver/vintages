################# Napa Vintage Dataset (PA) - 10/19/2020 #########################

#Housekeeping
rm(list=ls())
options(stringsAsFactors = FALSE)

#Reading in csv files
mydat <- read.csv("/Users/phoebeautio/Desktop/Napa_Vintage.csv", header=TRUE, na.strings=c(""," ","NA"))
head(mydat)

climdat <- read.csv("/Users/phoebeautio/Desktop/Napa_1990-2019.csv", header=TRUE, na.strings=c(""," ","NA"))

#Parsing cliamte dates


#Subsetting climate data (dates and columns of interest)
#April 1 - September 30
climdat2 <- 

#Subsetting by location (Napa/Sonoma)
napa <- mydat[which(mydat$Location=="Napa"), ]
  napa$Description_WS = NULL

sonoma <- mydat[which(mydat$Location=="Sonoma"), ]
  sonoma$Description_WS = NULL
  
#given some period of time, the output is summed GDD and/or summed precipitation 
  #I could use built in GDD code... then just sum that output
  #start with precipitation?
  #- using GDD over the growing season to predict quality.... summation of precipitation between April 1st and sep 30th
  #x year, y temp or precipitation
  
  # GDD base temp = 10
  climdat$TOBS <- ifelse(climdat$mean_temp >=10, climdat$mean_temp-10, 0)
  climdat$gdd <- ave(climdat$gddbase, climdat$year, FUN=cumsum)
  
  sum((climdat[i]-xm)^2) for i = 1 to i = 50
    
  My.Data[grep("^G45", My.Data$x), ]
  subset(My.Data, grepl("^G45", My.Data$x))
  
  for(i in 1:nrow(climdat)){
    if(isTRUE(grepl(pattern = "2010-11-11", x = climdat[i, "DATE"]))){ 
    print(i)
      }
    }
    
  #`If you have an object called daily_temp with daily temperature values:
    #specify the temperature threshold etc for forcing sum calculation (this is from a recent paper- happy to send a long the ):
    
    force = ifelse(daily_temp>5, 28.4/(1+exp(-.185*(daily_temp-18.4))),0)
  
  #add up all the forcing       
  
  forcesum <- sapply(1:ncol(force), function(x) (cumsum(force[1:nrow(force),x])))
  
  
  
