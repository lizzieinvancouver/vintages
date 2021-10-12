# Trialing daymet data to get daily estimates of percipitation and temperature for the Columbia Valley (in Oregan)
#for my project and Geoffs project

#started by Faith Jones Oct 06 2021 base off code for napa started Sep 29th 

rm(list = ls())

#load packages
library(raster) # work with singel layer raster maps 
library(sf)# work with shapefiles (Napa shape)
library(parallel) # for working with multiple cores



#Flaggs
midge <- TRUE
Trial <- FALSE
SaveMapFiles <-FALSE
SaveCSVFiles <- TRUE

#Read in data 
if(midge == TRUE){
	setwd("/home/data/Daymet/ColumbiaVOregan")
	#napa shape file - not teh final one, but teh obe I got from Nacho ages ago. for TRIAL
	will <- st_read("../../WinegrapeRegions/selectRegions/ColumbiaValleyOregan.shp")
	will1 <- will[1] #select just one layer 
	#Raster files 
	rasterFiles <- list.files(recursive = TRUE, pattern = ".nc")
	daymetFiles4 <- grep("tile4", rasterFiles, value = TRUE) # daymet Columbia Valley valley files
	daymetFiles5 <- grep("tile5", rasterFiles, value = TRUE) # daymet Columbia Valley files
	daymetFiles7 <- grep("tile7", rasterFiles, value = TRUE) # daymet Columbia Valley files
	daymetFiles8 <- grep("tile8", rasterFiles, value = TRUE) # daymet Columbia Valley files
	daymetFiles9  <- grep("tile9", rasterFiles, value = TRUE) # daymet Columbia Valley files


} else if(midge == FALSE){
	setwd("/home/faith/Documents/mnt/UBC/Wine/climate")
	#napa shape file - not teh final one, but teh obe I got from Nacho ages ago. for TRIAL
	will <- st_read("/home/faith/Documents/mnt/UBC/Wine/climate/RE_Winegrape_regions/selectRegions/ColumbiaValleyOregan.shp")
	will1 <- will[1] #select just one layer 
	#Raster files 
	rasterFiles <- list.files(recursive = TRUE, pattern = ".nc")
	daymetFiles4 <- grep("tile4", rasterFiles, value = TRUE) # daymet Columbia Valley files
	daymetFiles5 <- grep("tile5", rasterFiles, value = TRUE) # daymet Columbia Valley files
	daymetFiles7 <- grep("tile7", rasterFiles, value = TRUE) # daymet Columbia Valley files
	daymetFiles8 <- grep("tile8", rasterFiles, value = TRUE) # daymet Columbia Valley files
	daymetFiles9 <- grep("tile9", rasterFiles, value = TRUE) # daymet Columbia Valley files



}



#tmax 
rasterFilesMax24 <- daymetFiles4[grep("max",daymetFiles4)]#select rasters with teh word max in 
rasterFilesMax4 <- rasterFilesMax24[!rasterFilesMax24 %in% grep(".aux.xml",rasterFilesMax24, value = TRUE)]#Remove a weir d looking file
print(rasterFilesMax4)

rasterFilesMax25 <- daymetFiles5[grep("max",daymetFiles5)]#select rasters with teh word max in 
rasterFilesMax5 <- rasterFilesMax25[!rasterFilesMax25 %in% grep(".aux.xml",rasterFilesMax25, value = TRUE)]#Remove a weir d looking file
print(rasterFilesMax5)

rasterFilesMax27 <- daymetFiles7[grep("max",daymetFiles7)]#select rasters with teh word max in 
rasterFilesMax7 <- rasterFilesMax27[!rasterFilesMax27 %in% grep(".aux.xml",rasterFilesMax27, value = TRUE)]#Remove a weir d looking file
print(rasterFilesMax7)

rasterFilesMax28 <- daymetFiles8[grep("max",daymetFiles8)]#select rasters with teh word max in 
rasterFilesMax8 <- rasterFilesMax28[!rasterFilesMax28 %in% grep(".aux.xml",rasterFilesMax28, value = TRUE)]#Remove a weir d looking file
print(rasterFilesMax8)

rasterFilesMax29 <- daymetFiles9[grep("max",daymetFiles9)]#select rasters with teh word max in 
rasterFilesMax9 <- rasterFilesMax29[!rasterFilesMax29 %in% grep(".aux.xml",rasterFilesMax29, value = TRUE)]#Remove a weir d looking file

print(rasterFilesMax9)


#tmin
rasterFilesMin24 <- daymetFiles4[grep("min", daymetFiles4)]#select rasters with teh word min in 
rasterFilesMin4 <- rasterFilesMin24[!rasterFilesMin24 %in% grep(".aux.xml",rasterFilesMin24, value = TRUE)]

rasterFilesMin25 <- daymetFiles5[grep("min", daymetFiles5)]#select rasters with teh word min in 
rasterFilesMin5 <- rasterFilesMin25[!rasterFilesMin25 %in% grep(".aux.xml",rasterFilesMin25, value = TRUE)]

rasterFilesMin27 <- daymetFiles7[grep("min", daymetFiles7)]#select rasters with teh word min in 
rasterFilesMin7 <- rasterFilesMin27[!rasterFilesMin27 %in% grep(".aux.xml",rasterFilesMin27, value = TRUE)]

rasterFilesMin28 <- daymetFiles8[grep("min", daymetFiles8)]#select rasters with teh word min in 
rasterFilesMin8 <- rasterFilesMin28[!rasterFilesMin28 %in% grep(".aux.xml",rasterFilesMin28, value = TRUE)]

rasterFilesMin29 <- daymetFiles9[grep("min", daymetFiles9)]#select rasters with teh word min in 
rasterFilesMin9 <- rasterFilesMin29[!rasterFilesMin29 %in% grep(".aux.xml",rasterFilesMin29, value = TRUE)]


#Precipitation
rasterFilesPrcp24 <- daymetFiles4[grep("prcp", daymetFiles4)]#select rasters with teh word max in 
rasterFilesPrcp4 <- rasterFilesPrcp24[!rasterFilesPrcp24 %in% grep(".aux.xml",rasterFilesPrcp24, value = TRUE)]

rasterFilesPrcp25 <- daymetFiles5[grep("prcp", daymetFiles5)]#select rasters with teh word max in 
rasterFilesPrcp5 <- rasterFilesPrcp25[!rasterFilesPrcp25 %in% grep(".aux.xml",rasterFilesPrcp25, value = TRUE)]

rasterFilesPrcp27 <- daymetFiles7[grep("prcp", daymetFiles7)]#select rasters with teh word max in 
rasterFilesPrcp7 <- rasterFilesPrcp27[!rasterFilesPrcp27 %in% grep(".aux.xml",rasterFilesPrcp27, value = TRUE)]

rasterFilesPrcp28 <- daymetFiles8[grep("prcp", daymetFiles8)]#select rasters with teh word max in 
rasterFilesPrcp8 <- rasterFilesPrcp28[!rasterFilesPrcp28 %in% grep(".aux.xml",rasterFilesPrcp28, value = TRUE)]

rasterFilesPrcp29 <- daymetFiles9[grep("prcp", daymetFiles9)]#select rasters with teh word max in 
rasterFilesPrcp9 <- rasterFilesPrcp29[!rasterFilesPrcp29 %in% grep(".aux.xml",rasterFilesPrcp29, value = TRUE)]

#Set correct projection 
correctProjection <- "+proj=longlat +ellps=WGS84 +no_defs"

#Set core number
nCore <- detectCores()/2 


#Merge the tiles together 
#_------------------------------------

	raster36ListMax <- list()
	raster36ListMin <- list()
	raster36ListPrecp <- list()

if(Trial == TRUE){ # trial smaller version to get things working 

	for(i in 1:2){
		#i <-1
	#tmax
	raster4Max <- stack(rasterFilesMax4[i])
	raster5Max <- stack(rasterFilesMax5[i])
	raster7Max <- stack(rasterFilesMax7[i])
	raster8Max <- stack(rasterFilesMax8[i])
	raster9Max <- stack(rasterFilesMax9[i])
	raster36Max <- merge(raster4Max, raster5Max, raster7Max,raster8Max,raster9Max)
	names(raster36Max) <- names(raster4Max)
	raster36ListMax[[i]] <- raster36Max

	#Tmin
	raster4Min <- stack(rasterFilesMin4[i])
	raster5Min <- stack(rasterFilesMin5[i])
	raster7Min <- stack(rasterFilesMin7[i])
	raster8Min <- stack(rasterFilesMin8[i])
	raster9Min <- stack(rasterFilesMin9[i])
	raster36Min <- merge(raster4Min, raster5Min, raster7Min,raster8Min,raster9Min)
	names(raster36Min) <- names(raster4Min)
	raster36ListMin[[i]] <- raster36Min


	#Precipitation
	raster4Prec <- stack(rasterFilesPrcp4[i])
	raster5Prec <- stack(rasterFilesPrcp5[i])
	raster7Prec <- stack(rasterFilesPrcp7[i])
	raster8Prec <- stack(rasterFilesPrcp8[i])
	raster9Prec <- stack(rasterFilesPrcp9[i])
	raster36Prec <- merge(raster4Prec, raster5Prec, raster7Prec,raster8Prec,raster9Prec)
	names(raster36Prec) <- names(raster4Prec)
	raster36ListPrecp[[i]] <- raster36Prec
	}


}


if(Trial == FALSE){


	for(i in 1:length(rasterFilesMax4)){
				#i <-1
	#tmax
		raster4Max <- stack(rasterFilesMax4[i])
	raster5Max <- stack(rasterFilesMax5[i])
	raster7Max <- stack(rasterFilesMax7[i])
	raster8Max <- stack(rasterFilesMax8[i])
	raster9Max <- stack(rasterFilesMax9[i])
	raster36Max <- merge(raster4Max, raster5Max, raster7Max,raster8Max,raster9Max)
	names(raster36Max) <- names(raster4Max)
	raster36ListMax[[i]] <- raster36Max

	#Tmin
	raster4Min <- stack(rasterFilesMin4[i])
	raster5Min <- stack(rasterFilesMin5[i])
	raster7Min <- stack(rasterFilesMin7[i])
	raster8Min <- stack(rasterFilesMin8[i])
	raster9Min <- stack(rasterFilesMin9[i])
	raster36Min <- merge(raster4Min, raster5Min, raster7Min,raster8Min,raster9Min)
	names(raster36Min) <- names(raster4Min)
	raster36ListMin[[i]] <- raster36Min


	#Precipitation
	raster4Prec <- stack(rasterFilesPrcp4[i])
	raster5Prec <- stack(rasterFilesPrcp5[i])
	raster7Prec <- stack(rasterFilesPrcp7[i])
	raster8Prec <- stack(rasterFilesPrcp8[i])
	raster9Prec <- stack(rasterFilesPrcp9[i])
	raster36Prec <- merge(raster4Prec, raster5Prec, raster7Prec,raster8Prec,raster9Prec)
	names(raster36Prec) <- names(raster4Prec)
	raster36ListPrecp[[i]] <- raster36Prec
	}

}


#work up to processing multiple layers - look through each year and save seperate layers
#------------------------------------------

#Make a function to do the cleaning
projectAndCropList <- function(inputListNames){
	#a function that reads in a raster, converts it to the right projection, trims to a shapefile, and saves a file

		inputStack <- stack(inputListNames)
		inputStack2 <- projectRaster(inputStack, crs = "+proj=longlat +ellps=WGS84 +no_defs")
		inputStack3 <- crop(inputStack2, will1)
		inputStack4 <- mask(inputStack3, will1)
		return(inputStack4)
}

#Maximum values
#-------------------------------------------------
rasterMaxCrop <- lapply(raster36ListMax, FUN = projectAndCropList)#, mc.cores = nCore) # paraleliszed lapply 

#Minimum values
#----------------------------
rasterMinCrop <- lapply(raster36ListMin, FUN = projectAndCropList)#, mc.cores = nCore) # paraleliszed lapply 

#Precipitation 
#-------------------------------
rasterMaxPrcp <- lapply(raster36ListPrecp, FUN = projectAndCropList)#, mc.cores = nCore) # paraleliszed lapply 


#Write files
#----------------------------

if(SaveMapFiles == TRUE){
	for(i in 1:(length(rasterFilesMax4))){
		#i <- 1
		writeRaster(stack(rasterMaxCrop[[i]]), filename=paste0(paste0( "tmax/tmax_",i+1979), ".nc"), overwrite=TRUE, format="CDF") # FAITH: update your directory here
		writeRaster(stack(rasterMinCrop[[i]]), filename=paste0(paste0( "tmin/tmin_",i+1979), ".nc"), overwrite=TRUE, format="CDF") # FAITH: update your directory here
		writeRaster(stack(rasterMaxPrcp[[i]]), filename=paste0(paste0( "prcp/prcp_",i+1979), ".nc"), overwrite=TRUE, format="CDF") # FAITH: update your directory here
	}

}


#Get mean per layer
#--------------------------- 


#maximum temp 
dailyMax <- list()

	for(ip in 1:length(rasterFilesMax4)){
	
		dailyMaxTempNapa  <- cellStats(rasterMaxCrop[[ip]], stat='mean') # get mean per day per map
		maxdf <- data.frame(dailyMaxTempNapa)
		maxdf$date <- rownames(maxdf)
		rownames(maxdf) <- NULL
		dailyMax[[ip]] <- maxdf
	}

tMaxTrial <- do.call("rbind", dailyMax)

#Mean temperature
dailyMean <- list()

	for(ip in 1:length(rasterFilesMin4)){
	

		#get mean per cell

		#get mean accross cells
		rastersListMean <- list(c(rasterFilesMax4[[ip]], rasterFilesMin4[[ip]]))
		minMax <-  lapply(rastersListMean, stack)

		dailyMMTempNapa  <- cellStats(stack(minMax), stat='mean')	
		mmdf <- data.frame(dailyMMTempNapa)
		mmdf$date <- rownames(mmdf)
		rownames(mmdf) <- NULL
		dailyMean[[ip]] <- mmdf
	}

tMeanTrial <- do.call("rbind", dailyMean)

#minimum temp 
dailyMin <- list()

	for(ip in 1:length(rasterFilesMin4)){
	

		dailyMinTempNapa  <- cellStats(rasterMinCrop[[ip]], stat='mean') # get mean per day per map
		mindf <- data.frame(dailyMinTempNapa)
		mindf$date <- rownames(mindf)
		rownames(mindf) <- NULL
		dailyMin[[ip]] <- mindf
	}

tMinTrial <- do.call("rbind", dailyMin)


#precipitation 
dailyPrecp <- list()

	for(ip in 1:length(rasterFilesPrcp4)){
	
		dailyPrTempNapa  <- cellStats(rasterMaxPrcp[[ip]], stat='mean') # get mean per day per map
		prdf <- data.frame(dailyPrTempNapa)
		prdf$date <- rownames(prdf)
		rownames(prdf) <- NULL
		dailyPrecp[[ip]] <- prdf
	}

prTrial <- do.call("rbind", dailyPrecp)


if(SaveCSVFiles == TRUE){


	write.csv(prTrial, "prcp/dailyPrcpMeanColVal.csv")
	write.csv(tMinTrial, "tmin/dailyTminMeanColVal.csv")
	write.csv(tMaxTrial, "tmax/dailyTmaxMeanColVal.csv")


}