# Trialing daymet data to get daily estimates of percipitation and temperature for the Columbia Valley (in Oregan)
#for my project and Geoffs project

#started by Faith Jones Oct 06 2021 base off code for napa started Sep 29th 

rm(list = ls())

#load packages
library(raster) # work with singel layer raster maps 
library(sf)# work with shapefiles (Napa shape)
library(parallel) # for working with multiple cores

nCore <- detectCores()/2

#Make a function to do the cleaning
projectAndCropList <- function(inputListNames){
	#a function that reads in a raster, converts it to the right projection, trims to a shapefile, and saves a file

		inputStack <- stack(inputListNames)
		inputStack2 <- projectRaster(inputStack, crs = "+proj=longlat +ellps=WGS84 +no_defs")
		inputStack3 <- crop(inputStack2, will1)
		inputStack4 <- mask(inputStack3, will1)
		return(inputStack4)
}

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

rasterFilesMax25 <- daymetFiles5[grep("max",daymetFiles5)]#select rasters with teh word max in 
rasterFilesMax5 <- rasterFilesMax25[!rasterFilesMax25 %in% grep(".aux.xml",rasterFilesMax25, value = TRUE)]#Remove a weir d looking file

rasterFilesMax27 <- daymetFiles7[grep("max",daymetFiles7)]#select rasters with teh word max in 
rasterFilesMax7 <- rasterFilesMax27[!rasterFilesMax27 %in% grep(".aux.xml",rasterFilesMax27, value = TRUE)]#Remove a weir d looking file

rasterFilesMax28 <- daymetFiles8[grep("max",daymetFiles8)]#select rasters with teh word max in 
rasterFilesMax8 <- rasterFilesMax28[!rasterFilesMax28 %in% grep(".aux.xml",rasterFilesMax28, value = TRUE)]#Remove a weir d looking file

rasterFilesMax29 <- daymetFiles9[grep("max",daymetFiles9)]#select rasters with teh word max in 
rasterFilesMax9 <- rasterFilesMax29[!rasterFilesMax29 %in% grep(".aux.xml",rasterFilesMax29, value = TRUE)]#Remove a weir d looking file

maxFiles <- list(rasterFilesMax4, rasterFilesMax5,rasterFilesMax7,rasterFilesMax8,rasterFilesMax9)


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

minFiles <- list(rasterFilesMin4, rasterFilesMin5,rasterFilesMin7,rasterFilesMin8,rasterFilesMin9)


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

prcpFiles <- list(rasterFilesPrcp4, rasterFilesPrcp5,rasterFilesPrcp7,rasterFilesPrcp8,rasterFilesPrcp9)

#Make a list of all files that I can loop through
allFiles <- list(maxFiles,minFiles,prcpFiles)

#Set correct projection 
correctProjection <- "+proj=longlat +ellps=WGS84 +no_defs"

#Set core number
#work up to processing multiple layers - look through each year and save seperate layers
#------------------------------------------

#Get mean values in a loop to save having too much in the memory
#--------------------------------------------------------------------
#make a vector of file names 
filenames <- c( "tmax/tmax_","tmin/tmin_","prcp/prcp_")
namesLoop <- c("Max","Min", "Precp") #names used to assign a value to teh output of the loop 
saveCsvNames <- c("tmax/dailyTmaxMeanColVal.csv","tmin/dailyTminMeanColVal.csv","prcp/dailyPrcpMeanColVal.csv")


#combine lists of raster file names into a new list for looping
rasterList <- list()


for(iData in 1:3){
#Maximum values
	
	#iData <- 1
	
	#Merge the tiles together 
	#_------------------------------------

	for(i in 1:length(rasterFilesMax4)){ # combine all the maps together. i = year
		#i <-2
		raster4Max <- stack(allFiles[[iData]][[1]][i])
		raster5Max <- stack(allFiles[[iData]][[2]][i])
		raster7Max <- stack(allFiles[[iData]][[3]][i])
		raster8Max <- stack(allFiles[[iData]][[4]][i])
		raster9Max <- stack(allFiles[[iData]][[5]][i])
		raster36Max <- merge(raster4Max, raster5Max, raster7Max,raster8Max,raster9Max)
		names(raster36Max) <- names(raster4Max)
		rasterList[[i]] <- raster36Max
	}

	#-------------------------------------------------
	rasterCrop <-  lapply(rasterList, FUN = projectAndCropList)
	assign(paste("rasterCrop", namesLoop[[iData]], sep = ""),rasterCrop)#give this a name I can call outside of the loop


	#RiteFile (based of tmax code)
	#-------------------------------------------------

	#maximum temp 
	dailyMax <- list()

		for(ip in 1:length(rasterFilesMax4)){
		
			#ip <- 1
			dailyMaxTempNapa  <- cellStats(rasterCrop[[ip]], stat='mean') # get mean per day per map
			maxdf <- data.frame(dailyMaxTempNapa)
			maxdf$date <- rownames(maxdf)
			rownames(maxdf) <- NULL
			dailyMax[[ip]] <- maxdf
		}

	tMaxTrial <- do.call("rbind", dailyMax)
	
	write.csv(tMaxTrial, saveCsvNames[iData])

	rasterList <- list() # Empty the list at the end of the itteration so I don't use too much memory
}


#Write files
#----------------------------

if(SaveMapFiles == TRUE){
	for(i in 1:(length(rasterFilesMax4))){
		#i <- 1
		writeRaster(stack(rasterCropMax[[i]]), filename=paste0(paste0( "tmax/tmax_",i+1979), ".nc"), overwrite=TRUE, format="CDF") # FAITH: update your directory here
		writeRaster(stack(rasterCropMin[[i]]), filename=paste0(paste0( "tmin/tmin_",i+1979), ".nc"), overwrite=TRUE, format="CDF") # FAITH: update your directory here
		writeRaster(stack(rasterCropPrecp[[i]]), filename=paste0(paste0( "prcp/prcp_",i+1979), ".nc"), overwrite=TRUE, format="CDF") # FAITH: update your directory here
	}

}


