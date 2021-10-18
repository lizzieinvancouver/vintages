# Trialing daymet data to get daily estimates of percipitation and temperature for nappa
#for my project and Geoffs project

#started by Faith Jones Sep 21 2021

rm(list = ls())

#load packages
library(raster) # work with singel layer raster maps 
library(sf)# work with shapefiles (Napa shape)
library(parallel) # for working with multiple cores



#Flaggs
midge <- TRUE
Trial <- FALSE
SaveMapFiles <- FALSE
SaveCSVFiles <- TRUE

#Read in data 
if(midge == TRUE){
	setwd("/home/data/Daymet/California")
	#napa shape file - not teh final one, but teh obe I got from Nacho ages ago. for TRIAL
	napa <- st_read("../../WinegrapeRegions/selectRegions/napa_Narrow.shp")
	napa1 <- napa[1] #select just one layer 
	#Raster files 
	rasterFiles <- list.files(recursive = TRUE, pattern = ".nc")
	daymetFiles <- grep("CF_tarred/", rasterFiles, value = TRUE)


} else if(midge == FALSE){
	setwd("/home/faith/Documents/mnt/UBC/Wine/climate")
	#napa shape file - not teh final one, but teh obe I got from Nacho ages ago. for TRIAL
	napa <- st_read("/home/faith/Documents/mnt/UBC/Wine/climate/RE_Winegrape_regions/selectRegions/napa_Narrow.shp")
	napa1 <- napa[1] #select just one layer 
	#Raster files 
	rasterFiles <- list.files(recursive = TRUE, pattern = ".nc")
	daymetFiles <- grep("daymet", rasterFiles, value = TRUE)

}



print(daymetFiles)

rasterFilesMax2 <- rasterFiles[grep("max",daymetFiles)]#select rasters with teh word max in 
rasterFilesMax <- rasterFilesMax2[!rasterFilesMax2 %in% grep(".aux.xml",rasterFilesMax2, value = TRUE)]#Remove a weir d looking file
rasterFilesSubset <- rasterFilesMax[1:2]


rasterFilesMin2 <- rasterFiles[grep("min", daymetFiles)]#select rasters with teh word max in 
rasterFilesMin <- rasterFilesMin2[!rasterFilesMin2 %in% grep(".aux.xml",rasterFilesMin2, value = TRUE)]

rasterFilesPrcp2 <- rasterFiles[grep("prcp", daymetFiles)]#select rasters with teh word max in 
rasterFilesPrcp <- rasterFilesPrcp2[!rasterFilesPrcp2 %in% grep(".aux.xml",rasterFilesPrcp2, value = TRUE)]

#Set correct projection 
correctProjection <- "+proj=longlat +ellps=WGS84 +no_defs"

#Set core number
nCore <- detectCores()/2 



#work up to processing multiple layers - look through each year and save seperate layers
#------------------------------------------

#Make a function to do the cleaning
projectAndCropList <- function(inputListNames){
	#a function that reads in a raster, converts it to the right projection, trims to a shapefile, and saves a file

		inputStack <- stack(inputListNames)
		inputStack2 <- projectRaster(inputStack, crs = "+proj=longlat +ellps=WGS84 +no_defs")
		inputStack3 <- crop(inputStack2, napa1)
		inputStack4 <- mask(inputStack3, napa1)
		return(inputStack4)
}


#Maximum values
#-------------------------------------------------
rasterMaxCrop <- mclapply(rasterFilesMax, FUN = projectAndCropList, mc.cores = nCore) # paraleliszed lapply 


#Minimum values
#----------------------------
rasterMinCrop <- mclapply(rasterFilesMin, FUN = projectAndCropList, mc.cores = nCore) # paraleliszed lapply 

#Precipitation 
#-------------------------------
rasterMaxPrcp <- mclapply(rasterFilesPrcp, FUN = projectAndCropList, mc.cores = nCore) # paraleliszed lapply 


#Write files
#----------------------------

if(SaveMapFiles == TRUE){
	for(i in 1:(length(rasterFilesMax))){
		#i <- 1
		writeRaster(stack(rasterMaxCrop[[i]]), filename=paste0(paste0( "napa/tmax/tmax_",i+1979), ".nc"), overwrite=TRUE, format="CDF") # FAITH: update your directory here
		writeRaster(stack(rasterMinCrop[[i]]), filename=paste0(paste0( "napa/tmin/tmin_",i+1979), ".nc"), overwrite=TRUE, format="CDF") # FAITH: update your directory here
		writeRaster(stack(rasterMaxPrcp[[i]]), filename=paste0(paste0( "napa/prcp/prcp_",i+1979), ".nc"), overwrite=TRUE, format="CDF") # FAITH: update your directory here
	}

}



#Get mean per layer
#--------------------------- 

if(Trial == TRUE){

	rasterMaxCropTrial <-  mclapply(rasterFilesMax[1:2], FUN = projectAndCropList, mc.cores = 2) # paraleliszed lapply 
	rasterMainCropTrial <-  mclapply(rasterFilesMin[1:2], FUN = projectAndCropList, mc.cores = 2) # paraleliszed lapply 

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

	for(ip in 1:length(rasterFilesMax)){
	
		dailyMaxTempNapa  <- cellStats(rasterMaxCrop[[ip]], stat='mean') # get mean per day per map
		maxdf <- data.frame(dailyMaxTempNapa)
		maxdf$date <- rownames(maxdf)
		rownames(maxdf) <- NULL
		dailyMax[[ip]] <- maxdf
	}

tMaxTrial <- do.call("rbind", dailyMax)

#Mean temperature
dailyMean <- list()

	for(ip in 1:length(rasterFilesMin)){
	

		#get mean per cell

		#get mean accross cells
		rastersListMean <- list(c(rasterFilesMax[[ip]], rasterFilesMin[[ip]]))
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

	for(ip in 1:length(rasterFilesMin)){
	

		dailyMinTempNapa  <- cellStats(rasterMinCrop[[ip]], stat='mean') # get mean per day per map
		mindf <- data.frame(dailyMinTempNapa)
		mindf$date <- rownames(mindf)
		rownames(mindf) <- NULL
		dailyMin[[ip]] <- mindf
	}

tMinTrial <- do.call("rbind", dailyMin)


#precipitation 
dailyPrecp <- list()

	for(ip in 1:length(rasterMaxPrcp)){
	
		dailyPrTempNapa  <- cellStats(rasterMaxPrcp[[ip]], stat='mean') # get mean per day per map
		prdf <- data.frame(dailyPrTempNapa)
		prdf$date <- rownames(prdf)
		rownames(prdf) <- NULL
		dailyPrecp[[ip]] <- prdf
	}

prTrial <- do.call("rbind", dailyPrecp)


if(SaveCSVFiles == TRUE){


	write.csv(prTrial, "napa/prcp/dailyPrcpMeanNapa.csv")
	write.csv(tMinTrial, "napa/tmin/dailyTminMeanNapa.csv")
	write.csv(tMeanTrial, "napa/dailyTmeanMeanNapa.csv")
	write.csv(tMaxTrial, "napa/tmax/dailyTmaxMeanNapa.csv")


}