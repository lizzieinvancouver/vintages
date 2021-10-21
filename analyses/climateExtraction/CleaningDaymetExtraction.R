#Exploring teh mean climate variables Faith extracted from Daymet for the vintages project for different winegrape regions.
#The code to make these data are run on Midge, and they are stored in the ClimateExtraction folder of teh vintages project. 

#Started by Faith Jones Oct 19 2021 (teh day she ordered lab tshirts!)

rm(list = ls())


library(ggplot2)

setwd("/home/faith/Documents/github/vintages/analyses/climateExtraction")

#read in Napa data
napaMax <- read.csv("../input/climate/DaymetMeans/dailyTmaxMeanNapa.csv")
napaMin <- read.csv("../input/climate/DaymetMeans/dailyTminMeanNapa.csv")
napaPrcp <- read.csv("../input/climate/DaymetMeans/dailyPrcpMeanNapa.csv")
napaData <- merge(merge(napaMax, napaMin),napaPrcp)

#read in Sonoma Valley data
sonoMax <- read.csv("../input/climate/DaymetMeans/dailyTmaxMeanSonoma.csv")
sonoMin <- read.csv("../input/climate/DaymetMeans/dailyTminMeanSonoma.csv")
sonoPrcp <- read.csv("../input/climate/DaymetMeans/dailyPrcpMeanSonoma.csv")
sonoData <- merge(merge(sonoMax, sonoMin),sonoPrcp)

#read in North Coast (excluding Sonoma and Napa) data
nCoastMax <- read.csv("../input/climate/DaymetMeans/dailyTmaxMeannCoast.csv")
nCoastMin <- read.csv("../input/climate/DaymetMeans/dailyTminMeannCoast.csv")
nCoastPrcp <- read.csv("../input/climate/DaymetMeans/dailyPrcpMeannCoast.csv")
nCoastData <- merge(merge(nCoastMax, nCoastMin),nCoastPrcp)

#read in Willamet Valley data
willMax <- read.csv("../input/climate/DaymetMeans/dailyTmaxMeanWillamet.csv")
willMin <- read.csv("../input/climate/DaymetMeans/dailyTminMeanWillamet.csv")
willPrcp <- read.csv("../input/climate/DaymetMeans/dailyPrcpMeanWillamet.csv")
willData <- merge(merge(willMax, willMin),willPrcp)

#read in Columbia Valley data
colValMax <- read.csv("../input/climate/DaymetMeans/dailyTmaxMeanColVal.csv")
colValMin <- read.csv("../input/climate/DaymetMeans/dailyTminMeanColVal.csv")
colValPrcp <- read.csv("../input/climate/DaymetMeans/dailyPrcpMeanColVal.csv")
colValData <- merge(merge(colValMax, colValMin, by = c("date", "X")),colValPrcp, by = c("date", "X"))



#Clean the code a bit

tempBefore <- list(napaData, sonoData, nCoastData, willData, colValData)
tempDataList <- list()

regionNames <- c("napa", "sonoma", "nCoast", "willamet", "columbia")

for(i in 1:5){ # not columbia valley for now

	#Combine data into a single csv
	iData <- tempBefore[[i]]

	#Remove X columns 
	iData$X <- NULL
	iData$X <- NULL

	#rename columns
	names(iData) <- c("date", "Tmax", "Tmin", "Prcp")

	#Assign region column 
	iData$Region <- regionNames[i]

	#Make dates actual dates

	iData$date2 <- gsub("X", "", iData$date)
	iData$date2 <- as.Date(iData$date2, "%Y.%m.%d")
	iData$date <- iData$date2
	iData$date2 <- NULL


	#make mean value from min and max values
	iData$Tmean <- rowMeans(iData[,c("Tmin","Tmax")])

	tempDataList[[i]] <- iData


}

climData <- do.call("rbind", tempDataList)

tail(climData)

write.csv(climData, "../input/climate/DaymetMeans/climDailyMeans.csv")


