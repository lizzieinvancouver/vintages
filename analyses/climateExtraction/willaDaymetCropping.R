# Trialing daymet data to get daily estimates of percipitation and temperature for the Willamet Valley
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
SaveMapFiles <-TRUE
SaveCSVFiles <- TRUE

#Read in data 
if(midge == TRUE){
	setwd("/home/data/Daymet/willamet")
	#napa shape file - not teh final one, but teh obe I got from Nacho ages ago. for TRIAL
	will <- st_read("../../WinegrapeRegions/selectRegions/WillametValley.shp")
	will1 <- will[1] #select just one layer 
	#Raster files 
	rasterFiles <- list.files(recursive = TRUE, pattern = ".nc")
	daymetFiles3 <- grep("tile3", rasterFiles, value = TRUE) # daymet willamet valley files
	daymetFiles6 <- grep("tile6", rasterFiles, value = TRUE) # daymet willamet valley files


} else if(midge == FALSE){
	setwd("/home/faith/Documents/mnt/UBC/Wine/climate")
	#napa shape file - not teh final one, but teh obe I got from Nacho ages ago. for TRIAL
	will <- st_read("/home/faith/Documents/mnt/UBC/Wine/climate/RE_Winegrape_regions/selectRegions/WillametValley.shp")
	will1 <- will[1] #select just one layer 
	#Raster files 
	rasterFiles <- list.files(recursive = TRUE, pattern = ".nc")
	daymetFiles3 <- grep("tile3", rasterFiles, value = TRUE) # daymet willamet valley files
	daymetFiles6 <- grep("tile6", rasterFiles, value = TRUE) # daymet willamet valley files


}


#tmax 
rasterFilesMax23 <- daymetFiles3[grep("max",daymetFiles3)]#select rasters with teh word max in 
rasterFilesMax3 <- rasterFilesMax23[!rasterFilesMax23 %in% grep(".aux.xml",rasterFilesMax23, value = TRUE)]#Remove a weir d looking file

rasterFilesMax26 <- daymetFiles6[grep("max",daymetFiles6)]#select rasters with teh word max in 
rasterFilesMax6 <- rasterFilesMax26[!rasterFilesMax26 %in% grep(".aux.xml",rasterFilesMax26, value = TRUE)]#Remove a weir d looking file

#tmin
rasterFilesMin23 <- daymetFiles3[grep("min", daymetFiles3)]#select rasters with teh word min in 
rasterFilesMin3 <- rasterFilesMin23[!rasterFilesMin23 %in% grep(".aux.xml",rasterFilesMin23, value = TRUE)]

rasterFilesMin26 <- daymetFiles6[grep("min", daymetFiles6)]#select rasters with teh word min in 
rasterFilesMin6 <- rasterFilesMin26[!rasterFilesMin26 %in% grep(".aux.xml",rasterFilesMin26, value = TRUE)]


#Precipitation
rasterFilesPrcp23 <- daymetFiles3[grep("prcp", daymetFiles3)]#select rasters with teh word max in 
rasterFilesPrcp3 <- rasterFilesPrcp23[!rasterFilesPrcp23 %in% grep(".aux.xml",rasterFilesPrcp23, value = TRUE)]

rasterFilesPrcp26 <- daymetFiles6[grep("prcp", daymetFiles6)]#select rasters with teh word max in 
rasterFilesPrcp6 <- rasterFilesPrcp26[!rasterFilesPrcp26 %in% grep(".aux.xml",rasterFilesPrcp26, value = TRUE)]


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

	for(i in 1:3){
		#i <-1
	#tmax
	raster3Max <- stack(rasterFilesMax3[i])
	raster6Max <- stack(rasterFilesMax6[i])
	raster36Max <- merge(raster3Max, raster6Max)
	names(raster36Max) <- names(raster6Max)
	raster36ListMax[[i]] <- raster36Max

	#Tmin
	raster3Min <- stack(rasterFilesMin3[i])
	raster6Min <- stack(rasterFilesMin6[i])
	raster36Min <- merge(raster3Min, raster6Min)
	names(raster36Min) <- names(raster6Min)
	raster36ListMin[[i]] <- raster36Min


	#Precipitation
	raster3Prec <- stack(rasterFilesPrcp3[i])
	raster6Prec <- stack(rasterFilesPrcp6[i])
	raster36Prec <- merge(raster3Prec, raster6Prec)
	names(raster36Prec) <- names(raster6Prec)
	raster36ListPrecp[[i]] <- raster36Prec
	}


}

if(Trial == FALSE){


	for(i in 1:length(rasterFilesMax3)){
				#i <-1
	#tmax
	raster3Max <- stack(rasterFilesMax3[i])
	raster6Max <- stack(rasterFilesMax6[i])
	raster36Max <- merge(raster3Max, raster6Max)
	names(raster36Max) <- names(raster6Max)
	raster36ListMax[[i]] <- raster36Max

	#Tmin
	raster3Min <- stack(rasterFilesMin3[i])
	raster6Min <- stack(rasterFilesMin6[i])
	raster36Min <- merge(raster3Min, raster6Min)
	names(raster36Min) <- names(raster6Min)
	raster36ListMin[[i]] <- raster36Min


	#Precipitation
	raster3Prec <- stack(rasterFilesPrcp3[i])
	raster6Prec <- stack(rasterFilesPrcp6[i])
	raster36Prec <- merge(raster3Prec, raster6Prec)
	names(raster36Prec) <- names(raster6Prec)
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
	for(i in 1:(length(rasterFilesMax3))){
		#i <- 1
		writeRaster(stack(rasterMaxCrop[[i]]), filename=paste0(paste0( "willamet/tmax/tmax_",i+1979), ".nc"), overwrite=TRUE, format="CDF") # FAITH: update your directory here
		writeRaster(stack(rasterMinCrop[[i]]), filename=paste0(paste0( "willamet/tmin/tmin_",i+1979), ".nc"), overwrite=TRUE, format="CDF") # FAITH: update your directory here
		writeRaster(stack(rasterMaxPrcp[[i]]), filename=paste0(paste0( "willamet/prcp/prcp_",i+1979), ".nc"), overwrite=TRUE, format="CDF") # FAITH: update your directory here
	}

}


#Get mean per layer
#--------------------------- 

if(Trial == TRUE){

	rasterMaxCropTrial <-  mclapply(rasterFilesMax3[1:2], FUN = projectAndCropList, mc.cores = 2) # paraleliszed lapply 
	rasterMainCropTrial <-  mclapply(rasterFilesMax3[1:2], FUN = projectAndCropList, mc.cores = 2) # paraleliszed lapply 
	rasterMaxCropTrial <- lapply(rasterFilesMax3[1:2], FUN = projectAndCropList)

	names(rasterMaxCropTrial) <- c(1,2)

	dailyMax <- list()

	for(ip in 1:length(rasterMaxCropTrial)){
	

		#get mean per cell

		#get mean accross cells
		rastersList <- list(c(rasterMaxCropTrial[[ip]], rasterMainCropTrial[[ip]]))
		minMax <-  lapply(rastersList, stack)

		dailyMaxTempNapa  <- cellStats(stack(minMax), stat='mean')	
		maxdf <- data.frame(dailyMaxTempNapa)
		maxdf$date <- rownames(maxdf)
		rownames(maxdf) <- NULL
		dailyMax[[ip]] <- maxdf
	}

	tMaxTrial <- do.call("rbind", dailyMax)
}


#maximum temp 
dailyMax <- list()

	for(ip in 1:length(rasterFilesMax3)){
	
		dailyMaxTempNapa  <- cellStats(rasterMaxCrop[[ip]], stat='mean') # get mean per day per map
		maxdf <- data.frame(dailyMaxTempNapa)
		maxdf$date <- rownames(maxdf)
		rownames(maxdf) <- NULL
		dailyMax[[ip]] <- maxdf
	}

tMaxTrial <- do.call("rbind", dailyMax)

#Mean temperature
dailyMean <- list()

	for(ip in 1:length(rasterFilesMin3)){
	

		#get mean per cell

		#get mean accross cells
		rastersListMean <- list(c(rasterFilesMax3[[ip]], rasterFilesMin3[[ip]]))
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

	for(ip in 1:length(rasterFilesMin3)){
	

		dailyMinTempNapa  <- cellStats(rasterMinCrop[[ip]], stat='mean') # get mean per day per map
		mindf <- data.frame(dailyMinTempNapa)
		mindf$date <- rownames(mindf)
		rownames(mindf) <- NULL
		dailyMin[[ip]] <- mindf
	}

tMinTrial <- do.call("rbind", dailyMin)


#precipitation 
dailyPrecp <- list()

	for(ip in 1:length(rasterFilesPrcp3)){
	
		dailyPrTempNapa  <- cellStats(rasterMaxPrcp[[ip]], stat='mean') # get mean per day per map
		prdf <- data.frame(dailyPrTempNapa)
		prdf$date <- rownames(prdf)
		rownames(prdf) <- NULL
		dailyPrecp[[ip]] <- prdf
	}

prTrial <- do.call("rbind", dailyPrecp)


if(SaveCSVFiles == TRUE){


	write.csv(prTrial, "willamet/prcp/dailyPrcpMeanWillamet.csv")
	write.csv(tMinTrial, "willamet/tmin/dailyTminMeanWillamet.csv")
	write.csv(tMaxTrial, "willamet/tmax/dailyTmaxMeanWillamet.csv")


}

