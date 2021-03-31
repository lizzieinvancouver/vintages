################# Napa Vintage Dataset (PA) - Updated 1/19/2021 #########################

#Note: st_helena is missing 1990 May... should we delete?

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


#Adding Phenology Column (by Variety)
  st_hosp['phen_stage'] <- NA
  st_hosp['phen_stage_cab'] <- NA
  st_hosp['phen_stage_chard'] <- NA
  st_hosp['phen_stage_mer'] <- NA
  #st_hosp['phen_stage_zin'] <- NA
  #st_hosp['phen_stage_rhone'] <- NA
    
  st_helena['phen_stage'] <- NA
  st_helena['phen_stage_cab'] <- NA
  st_helena['phen_stage_chard'] <- NA
  st_helena['phen_stage_mer'] <- NA
  #st_helena['phen_stage_zin'] <- NA
  #st_helena['phen_stage_rhone'] <- NA
  
  #General
    #Budburst (1)
    st_hosp$phen_stage[which(st_hosp$month=="4")] <- "1"
    st_hosp$phen_stage[which(st_hosp$month=="5" & st_hosp$day %in% c("1","2","3","4","5","6","7","8","9","10","11","12","13","14","15"))] <- "1"
   
    st_helena$phen_stage[which(st_helena$month=="4")] <- "1"
    st_helena$phen_stage[which(st_helena$month=="5" & st_helena$day %in% c("1","2","3","4","5","6","7","8","9","10","11","12","13","14","15"))] <- "1"
    
    #Bloom (2)
    st_hosp$phen_stage[which(st_hosp$month=="5" & st_hosp$day %in% c("16","17","18","19","20","21","22",
                                                                     "23","24","25","26","27","28","29","30","31"))] <- "2"
    st_hosp$phen_stage[which(st_hosp$month=="6")] <- "2"
    st_hosp$phen_stage[which(st_hosp$month=="7" & st_hosp$day %in% c("1","2","3","4","5","6","7","8","9",
                                                                     "10","11","12","13","14","15","16",
                                                                     "17","18","19","20","21","22","23"))] <- "2"
    st_helena$phen_stage[which(st_helena$month=="5" & st_helena$day %in% c("16","17","18","19","20","21","22",
                                                                           "23","24","25","26","27","28","29","30","31"))] <- "2"
    st_helena$phen_stage[which(st_helena$month=="6")] <- "2"
    st_helena$phen_stage[which(st_helena$month=="7" & st_helena$day %in% c("1","2","3","4","5","6","7","8","9",
                                                                     "10","11","12","13","14","15","16",
                                                                     "17","18","19","20","21","22","23"))] <- "2"
    #Veraison (3)
    st_hosp$phen_stage[which(st_hosp$month=="7" & st_hosp$day %in% c("24","25","26","27","28","29","30","31"))] <- "3"
    st_hosp$phen_stage[which(st_hosp$month=="8")] <- "3"
    st_hosp$phen_stage[which(st_hosp$month=="9" & st_hosp$day %in% c("1","2","3","4","5","6","7","8","9",
                                                                       "10","11","12","13","14","15"))] <- "3"
    
    st_helena$phen_stage[which(st_helena$month=="7" & st_helena$day %in% c("24","25","26","27","28","29","30","31"))] <- "3"
    st_helena$phen_stage[which(st_helena$month=="8")] <- "3"
    st_helena$phen_stage[which(st_helena$month=="9" & st_helena$day %in% c("1","2","3","4","5","6","7","8","9",
                                                                     "10","11","12","13","14","15"))] <- "3"
    
  #Chardonnay
    #Budburst (1)
    st_hosp$phen_stage_chard[which(st_hosp$month=="3" & st_hosp$day %in% c("16","17","18","19","20","21","22",
                                                                           "23","24","25","26","27","28","29","30","31"))] <- "1"
    st_hosp$phen_stage_chard[which(st_hosp$month=="4")] <- "1"
    st_hosp$phen_stage_chard[which(st_hosp$month=="5" & st_hosp$day %in% c("1","2","3","4","5"))] <- "1"
    
    st_helena$phen_stage_chard[which(st_helena$month=="3" & st_helena$day %in% c("16","17","18","19","20","21","22",
                                                                                 "23","24","25","26","27","28","29","30","31"))] <- "1"
    st_helena$phen_stage_chard[which(st_helena$month=="4")] <- "1"
    st_helena$phen_stage_chard[which(st_helena$month=="5" & st_helena$day %in% c("1","2","3","4","5"))] <- "1"
    
    #Bloom (2)
    st_hosp$phen_stage_chard[which(st_hosp$month=="5" & st_hosp$day %in% c("6","7","8","9","10","11","12","13","14","15","16","17","18","19","20","21","22",
                                                                     "23","24","25","26","27","28","29","30","31"))] <- "2"
    st_hosp$phen_stage_chard[which(st_hosp$month=="6")] <- "2"
    st_hosp$phen_stage_chard[which(st_hosp$month=="7" & st_hosp$day %in% c("1","2","3","4","5","6","7","8","9","10"))] <- "2"
    
    
    st_helena$phen_stage_chard[which(st_helena$month=="5" & st_helena$day %in% c("6","7","8","9","10","11","12","13","14","15","16","17","18","19","20","21","22",
                                                                           "23","24","25","26","27","28","29","30","31"))] <- "2"
    st_helena$phen_stage_chard[which(st_helena$month=="6")] <- "2"
    st_helena$phen_stage_chard[which(st_helena$month=="7" & st_helena$day %in% c("1","2","3","4","5","6","7","8","9","10"))] <- "2"
    
    #Veraison (3)
    st_hosp$phen_stage_chard[which(st_hosp$month=="7" & st_hosp$day %in% c("11","12","13","14","15","16","17","18","19","20","21","22","23",
                                                                           "24","25","26","27","28","29","30","31"))] <- "3"
    st_hosp$phen_stage_chard[which(st_hosp$month=="8")] <- "3"
    st_hosp$phen_stage_chard[which(st_hosp$month=="9" & st_hosp$day %in% c("1","2","3","4","5","6","7","8","9",
                                                                     "10","11","12","13","14","15"))] <- "3"
    
    st_helena$phen_stage_chard[which(st_helena$month=="7" & st_helena$day %in% c("11","12","13","14","15","16","17","18","19","20","21","22","23",
                                                                                 "24","25","26","27","28","29","30","31"))] <- "3"
    st_helena$phen_stage_chard[which(st_helena$month=="8")] <- "3"
    st_helena$phen_stage_chard[which(st_helena$month=="9" & st_helena$day %in% c("1","2","3","4","5","6","7","8","9",
                                                                           "10","11","12","13","14","15"))] <- "3"
    
  #Cabernet
    #Budburst (1)
    st_hosp$phen_stage_cab[which(st_hosp$month=="3" & st_hosp$day %in% c("25","26","27","28","29","30","31"))] <- "1"
    st_hosp$phen_stage_cab[which(st_hosp$month=="4")] <- "1"
    st_hosp$phen_stage_cab[which(st_hosp$month=="5" & st_hosp$day %in% c("1","2","3","4","5","6","7","8","9","10","11","12","13","14","15"))] <- "1"
    
    st_helena$phen_stage_cab[which(st_helena$month=="3" & st_helena$day %in% c("25","26","27","28","29","30","31"))] <- "1"
    st_helena$phen_stage_cab[which(st_helena$month=="4")] <- "1"
    st_helena$phen_stage_cab[which(st_helena$month=="5" & st_helena$day %in% c("1","2","3","4","5","6","7","8","9","10","11","12","13","14","15"))] <- "1"
    
    #Bloom (2)
    st_hosp$phen_stage_cab[which(st_hosp$month=="5" & st_hosp$day %in% c("16","17","18","19","20","21","22",
                                                                     "23","24","25","26","27","28","29","30","31"))] <- "2"
    st_hosp$phen_stage_cab[which(st_hosp$month=="6")] <- "2"
    st_hosp$phen_stage_cab[which(st_hosp$month=="7" & st_hosp$day %in% c("1","2","3","4","5","6","7","8","9",
                                                                     "10","11","12","13","14","15","16",
                                                                     "17","18","19","20","21","22","23"))] <- "2"
    
    
    st_helena$phen_stage_cab[which(st_helena$month=="5" & st_helena$day %in% c("16","17","18","19","20","21","22",
                                                                           "23","24","25","26","27","28","29","30","31"))] <- "2"
    st_helena$phen_stage_cab[which(st_helena$month=="6")] <- "2"
    st_helena$phen_stage_cab[which(st_helena$month=="7" & st_helena$day %in% c("1","2","3","4","5","6","7","8","9",
                                                                           "10","11","12","13","14","15","16",
                                                                           "17","18","19","20","21","22","23"))] <- "2"
    #Veraison (3)
    st_hosp$phen_stage_cab[which(st_hosp$month=="7" & st_hosp$day %in% c("23","24","25","26","27","28","29","30","31"))] <- "3"
    st_hosp$phen_stage_cab[which(st_hosp$month=="8")] <- "3"
    st_hosp$phen_stage_cab[which(st_hosp$month=="9" & st_hosp$day %in% c("1","2","3","4","5","6","7","8","9",
                                                                     "10","11","12","13","14","15","16",
                                                                     "17","18","19","20","21","22","23"))] <- "3"
    
    st_helena$phen_stage_cab[which(st_helena$month=="7" & st_helena$day %in% c("23","24","25","26","27","28","29","30","31"))] <- "3"
    st_helena$phen_stage_cab[which(st_helena$month=="8")] <- "3"
    st_helena$phen_stage_cab[which(st_helena$month=="9" & st_helena$day %in% c("1","2","3","4","5","6","7","8","9",
                                                                           "10","11","12","13","14","15","16",
                                                                           "17","18","19","20","21","22","23"))] <- "3"
  #Merlot
    #Budburst (1)
    st_hosp$phen_stage_mer[which(st_hosp$month=="3" & st_hosp$day %in% c("25","26","27","28","29","30","31"))] <- "1"
    st_hosp$phen_stage_mer[which(st_hosp$month=="4")] <- "1"
    st_hosp$phen_stage_mer[which(st_hosp$month=="5" & st_hosp$day %in% c("1","2","3","4","5","6","7","8","9","10","11","12","13"))] <- "1"
    
    st_helena$phen_stage_mer[which(st_helena$month=="3" & st_helena$day %in% c("25","26","27","28","29","30","31"))] <- "1"
    st_helena$phen_stage_mer[which(st_helena$month=="4")] <- "1"
    st_helena$phen_stage_mer[which(st_helena$month=="5" & st_helena$day %in% c("1","2","3","4","5","6","7","8","9","10","11","12","13"))] <- "1"
    
    #Bloom (2)
    st_hosp$phen_stage_mer[which(st_hosp$month=="5" & st_hosp$day %in% c("14","15","16","17","18","19","20","21","22",
                                                                     "23","24","25","26","27","28","29","30","31"))] <- "2"
    st_hosp$phen_stage_mer[which(st_hosp$month=="6")] <- "2"
    st_hosp$phen_stage_mer[which(st_hosp$month=="7" & st_hosp$day %in% c("1","2","3","4","5","6","7","8","9",
                                                                     "10","11","12","13","14","15"))] <- "2"
    
    
    st_helena$phen_stage_mer[which(st_helena$month=="5" & st_helena$day %in% c("14","15","16","17","18","19","20","21","22",
                                                                           "23","24","25","26","27","28","29","30","31"))] <- "2"
    st_helena$phen_stage_mer[which(st_helena$month=="6")] <- "2"
    st_helena$phen_stage_mer[which(st_helena$month=="7" & st_helena$day %in% c("1","2","3","4","5","6","7","8","9",
                                                                           "10","11","12","13","14","15"))] <- "2"
    #Veraison (3)
    st_hosp$phen_stage_mer[which(st_hosp$month=="7" & st_hosp$day %in% c("16","17","18","19","20","21","22","23","24","25",
                                                                         "26","27","28","29","30","31"))] <- "3"
    st_hosp$phen_stage_mer[which(st_hosp$month=="8")] <- "3"
    st_hosp$phen_stage_mer[which(st_hosp$month=="9" & st_hosp$day %in% c("1","2","3","4","5","6","7","8","9",
                                                                     "10","11","12","13","14","15"))] <- "3"
    
    st_helena$phen_stage_mer[which(st_helena$month=="7" & st_helena$day %in% c("16","17","18","19","20","21","22","23","24","25",
                                                                               "26","27","28","29","30","31"))] <- "3"
    st_helena$phen_stage_mer[which(st_helena$month=="8")] <- "3"
    st_helena$phen_stage_mer[which(st_helena$month=="9" & st_helena$day %in% c("1","2","3","4","5","6","7","8","9",
                                                                           "10","11","12","13","14","15"))] <- "3"
    #remove empty rows
    st_hosp <- st_hosp[!(is.na(st_hosp$phen_stage) & is.na(st_hosp$phen_stage_cab) & is.na(st_hosp$phen_stage_chard) & is.na(st_hosp$phen_stage_mer)), ]
    st_helena <- st_helena[!(is.na(st_helena$phen_stage) & is.na(st_helena$phen_stage_cab) & is.na(st_helena$phen_stage_chard) & is.na(st_helena$phen_stage_mer)), ]

#Calculating GDD at each location -- GDD base temp = 10
  #Base GDD values - need individual per variety
    #general
    st_helena$gddbase <- ifelse(!is.na(st_helena$phen_stage) & st_helena$TAVG >= 10, st_helena$TAVG - 10, 0)
    st_hosp$gddbase <- ifelse(!is.na(st_hosp$phen_stage) & st_hosp$TAVG >= 10, st_hosp$TAVG - 10, 0)
    #chardonnay
    st_helena$gddbase_chard <- ifelse(!is.na(st_helena$phen_stage_chard) & st_helena$TAVG >= 10, st_helena$TAVG - 10, 0)
    st_hosp$gddbase_chard <- ifelse(!is.na(st_hosp$phen_stage_chard) & st_hosp$TAVG >= 10, st_hosp$TAVG - 10, 0)
    #cabernet
    st_helena$gddbase_cab <- ifelse(!is.na(st_helena$phen_stage_cab) & st_helena$TAVG >= 10, st_helena$TAVG - 10, 0)
    st_hosp$gddbase_cab <- ifelse(!is.na(st_hosp$phen_stage_cab) & st_hosp$TAVG >= 10, st_hosp$TAVG - 10, 0)
    #merlot
    st_helena$gddbase_mer <- ifelse(!is.na(st_helena$phen_stage_mer) & st_helena$TAVG >= 10, st_helena$TAVG - 10, 0)
    st_hosp$gddbase_mer <- ifelse(!is.na(st_hosp$phen_stage_mer) & st_hosp$TAVG >= 10, st_hosp$TAVG - 10, 0)
    
  #Summation by phenological stage and variety
    #general
    st_helena$gdd_phen <- ave(st_helena$gddbase, by = list(st_helena$year, st_helena$phen_stage), FUN = cumsum)
    st_hosp$gdd_phen <- ave(st_hosp$gddbase, by = list(st_hosp$year, st_hosp$phen_stage), FUN = cumsum)
    #merlot
    st_helena$gdd_phen_mer <- ave(st_helena$gddbase_mer, by = list(st_helena$year, st_helena$phen_stage_mer), FUN = cumsum)
    st_hosp$gdd_phen_mer <- ave(st_hosp$gddbase_mer, by = list(st_hosp$year, st_hosp$phen_stage_mer), FUN = cumsum)
    #chardonnay
    st_helena$gdd_phen_chard <- ave(st_helena$gddbase_chard, by = list(st_helena$year, st_helena$phen_stage_chard), FUN = cumsum)
    st_hosp$gdd_phen_chard <- ave(st_hosp$gddbase_chard, by = list(st_hosp$year, st_hosp$phen_stage_chard), FUN = cumsum)
    #cabernet
    st_helena$gdd_phen_cab <- ave(st_helena$gddbase_cab, by = list(st_helena$year, st_helena$phen_stage_cab), FUN = cumsum)
    st_hosp$gdd_phen_cab <- ave(st_hosp$gddbase_cab, by = list(st_hosp$year, st_hosp$phen_stage_cab), FUN = cumsum)
  
  #Summation by year and variety
    #general
    st_helena$gdd_year <- ave(st_helena$gddbase, st_helena$year, FUN = cumsum)
    st_hosp$gdd_year <- ave(st_hosp$gddbase, st_hosp$year, FUN = cumsum)
    #merlot
    st_helena$gdd_year_mer <- ave(st_helena$gddbase_mer, st_helena$year, FUN = cumsum)
    st_hosp$gdd_year_mer <- ave(st_hosp$gddbase_mer, st_hosp$year, FUN = cumsum)
    #chardonnay
    st_helena$gdd_year_chard <- ave(st_helena$gddbase_chard, st_helena$year, FUN = cumsum)
    st_hosp$gdd_year_chard <- ave(st_hosp$gddbase_chard, st_hosp$year, FUN = cumsum)
    #cabernet
    st_helena$gdd_year_cab <- ave(st_helena$gddbase_cab, st_helena$year, FUN = cumsum)
    st_hosp$gdd_year_cab <- ave(st_hosp$gddbase_cab, st_hosp$year, FUN = cumsum)
    
#creating dataframe of agg gdd from each variety by year and phen stage at each location
    
  #Phen Stage
    #general
    ag_st_helena_phen_gen <- aggregate(st_helena$gddbase, by = list(st_helena$year, st_helena$phen_stage), FUN=sum) 
    colnames(ag_st_helena_phen_gen)[1] <- c("Vintage")
    colnames(ag_st_helena_phen_gen)[2] <- c("phen_stage")
    colnames(ag_st_helena_phen_gen)[3] <- c("st_helena_gdd")
    
    ag_st_hosp_phen_gen <- aggregate(st_hosp$gddbase, by = list(st_hosp$year, st_hosp$phen_stage), FUN=sum) 
    colnames(ag_st_hosp_phen_gen)[1] <- c("Vintage")
    colnames(ag_st_hosp_phen_gen)[2] <- c("phen_stage")
    colnames(ag_st_hosp_phen_gen)[3] <- c("st_hosp_gdd")
    
    #cabernet
    ag_st_helena_phen_cab <- aggregate(st_helena$gddbase_cab, by = list(st_helena$year, st_helena$phen_stage_cab), FUN=sum) 
    colnames(ag_st_helena_phen_cab)[1] <- c("Vintage")
    colnames(ag_st_helena_phen_cab)[2] <- c("phen_stage")
    colnames(ag_st_helena_phen_cab)[3] <- c("st_helena_gdd_cab")
    
    ag_st_hosp_phen_cab <- aggregate(st_hosp$gddbase_cab, by = list(st_hosp$year, st_hosp$phen_stage_cab), FUN=sum) 
    colnames(ag_st_hosp_phen_cab)[1] <- c("Vintage")
    colnames(ag_st_hosp_phen_cab)[2] <- c("phen_stage")
    colnames(ag_st_hosp_phen_cab)[3] <- c("st_hosp_gdd_cab")
    
    #chardonnay
    ag_st_helena_phen_chard <- aggregate(st_helena$gddbase_chard, by = list(st_helena$year, st_helena$phen_stage_chard), FUN=sum) 
    colnames(ag_st_helena_phen_chard)[1] <- c("Vintage")
    colnames(ag_st_helena_phen_chard)[2] <- c("phen_stage")
    colnames(ag_st_helena_phen_chard)[3] <- c("st_helena_gdd_chard")
    
    ag_st_hosp_phen_chard <- aggregate(st_hosp$gddbase_chard, by = list(st_hosp$year, st_hosp$phen_stage_chard), FUN=sum) 
    colnames(ag_st_hosp_phen_chard)[1] <- c("Vintage")
    colnames(ag_st_hosp_phen_chard)[2] <- c("phen_stage")
    colnames(ag_st_hosp_phen_chard)[3] <- c("st_hosp_gdd_chard")
    
    #merlot
    ag_st_helena_phen_mer <- aggregate(st_helena$gddbase, by = list(st_helena$year, st_helena$phen_stage), FUN=sum) 
    colnames(ag_st_helena_phen_mer)[1] <- c("Vintage")
    colnames(ag_st_helena_phen_mer)[2] <- c("phen_stage")
    colnames(ag_st_helena_phen_mer)[3] <- c("st_helena_gdd_mer")
    
    ag_st_hosp_phen_mer <- aggregate(st_hosp$gddbase_mer, by = list(st_hosp$year, st_hosp$phen_stage_mer), FUN=sum) 
    colnames(ag_st_hosp_phen_mer)[1] <- c("Vintage")
    colnames(ag_st_hosp_phen_mer)[2] <- c("phen_stage")
    colnames(ag_st_hosp_phen_mer)[3] <- c("st_hosp_gdd_mer")
    
    gdd_agg_phen <- merge(ag_st_helena_phen_gen, ag_st_hosp_phen_gen)
    gdd_agg_phen <- merge(gdd_agg_phen, ag_st_hosp_phen_cab)
    gdd_agg_phen <- merge(gdd_agg_phen, ag_st_helena_phen_cab)
    gdd_agg_phen <- merge(gdd_agg_phen, ag_st_hosp_phen_chard)
    gdd_agg_phen <- merge(gdd_agg_phen, ag_st_helena_phen_chard)
    gdd_agg_phen <- merge(gdd_agg_phen, ag_st_hosp_phen_mer)
    gdd_agg_phen <- merge(gdd_agg_phen, ag_st_helena_phen_mer)
    
    #averaging by location for each variety
    gdd_agg_phen$gdd_avg_phen_gen <- rowMeans(gdd_agg_phen[, 3:4])
    gdd_agg_phen$gdd_avg_phen_cab <- rowMeans(gdd_agg_phen[, 5:6])
    gdd_agg_phen$gdd_avg_phen_chard <- rowMeans(gdd_agg_phen[, 7:8])
    gdd_agg_phen$gdd_avg_phen_mer <- rowMeans(gdd_agg_phen[, 9:10])
    
  #Year
    #gen
    ag_st_helena_year_gen <- aggregate(st_helena$gddbase, by = list(st_helena$year), FUN=sum) 
    colnames(ag_st_helena_year_gen)[1] <- c("Vintage")
    colnames(ag_st_helena_year_gen)[2] <- c("st_helena_gdd")

    ag_st_hosp_year_gen <- aggregate(st_hosp$gddbase, by = list(st_hosp$year), FUN=sum) 
    colnames(ag_st_hosp_year_gen)[1] <- c("Vintage")
    colnames(ag_st_hosp_year_gen)[2] <- c("st_hosp_gdd")
    
    #cabernet
    ag_st_helena_year_cab <- aggregate(st_helena$gddbase_cab, by = list(st_helena$year), FUN=sum) 
    colnames(ag_st_helena_year_cab)[1] <- c("Vintage")
    colnames(ag_st_helena_year_cab)[2] <- c("st_helena_gdd_cab")
    
    ag_st_hosp_year_cab <- aggregate(st_hosp$gddbase_cab, by = list(st_hosp$year), FUN=sum) 
    colnames(ag_st_hosp_year_cab)[1] <- c("Vintage")
    colnames(ag_st_hosp_year_cab)[2] <- c("st_hosp_gdd_cab")
    
    #chardonnay
    ag_st_helena_year_chard <- aggregate(st_helena$gddbase_chard, by = list(st_helena$year), FUN=sum) 
    colnames(ag_st_helena_year_chard)[1] <- c("Vintage")
    colnames(ag_st_helena_year_chard)[2] <- c("st_helena_gdd_chard")
    
    ag_st_hosp_year_chard <- aggregate(st_hosp$gddbase_chard, by = list(st_hosp$year), FUN=sum) 
    colnames(ag_st_hosp_year_chard)[1] <- c("Vintage")
    colnames(ag_st_hosp_year_chard)[2] <- c("st_hosp_gdd_chard")
    
    #merlot
    ag_st_helena_year_mer <- aggregate(st_helena$gddbase_mer, by = list(st_helena$year), FUN=sum) 
    colnames(ag_st_helena_year_mer)[1] <- c("Vintage")
    colnames(ag_st_helena_year_mer)[2] <- c("st_helena_gdd_mer")
    
    ag_st_hosp_year_mer <- aggregate(st_hosp$gddbase_mer, by = list(st_hosp$year), FUN=sum) 
    colnames(ag_st_hosp_year_mer)[1] <- c("Vintage")
    colnames(ag_st_hosp_year_mer)[2] <- c("st_hosp_gdd_mer")
    
    gdd_agg_year <- merge(ag_st_helena_year_gen, ag_st_hosp_year_gen)
    gdd_agg_year <- merge(gdd_agg_year, ag_st_helena_year_cab)
    gdd_agg_year <- merge(gdd_agg_year, ag_st_hosp_year_cab)
    gdd_agg_year <- merge(gdd_agg_year, ag_st_helena_year_chard)
    gdd_agg_year <- merge(gdd_agg_year, ag_st_hosp_year_chard)
    gdd_agg_year <- merge(gdd_agg_year, ag_st_helena_year_mer)
    gdd_agg_year <- merge(gdd_agg_year, ag_st_hosp_year_mer)
    
    #averaging by location for each variety
    gdd_agg_year$gdd_avg_year_gen <- rowMeans(gdd_agg_year[, 2:3])
    gdd_agg_year$gdd_avg_year_cab <- rowMeans(gdd_agg_year[, 4:5])
    gdd_agg_year$gdd_avg_year_chard <- rowMeans(gdd_agg_year[, 6:7])
    gdd_agg_year$gdd_avg_year_mer <- rowMeans(gdd_agg_year[, 8:9])

#Calculating Precipitation at each location by year and phen stage
    
  #Base GDD values - need individual per variety
    #general
    st_helena$prcpbase_gen <- ifelse(is.na(st_helena$phen_stage), 0, st_helena$PRCP)
    st_hosp$prcpbase_gen <- ifelse(is.na(st_hosp$phen_stage), 0, st_hosp$PRCP)
    #chardonnay
    st_helena$prcpbase_chard <- ifelse(is.na(st_helena$phen_stage_chard), 0, st_helena$PRCP)
    st_hosp$prcpbase_chard <- ifelse(is.na(st_hosp$phen_stage_chard), 0, st_hosp$PRCP)
    #cabernet
    st_helena$prcpbase_cab <- ifelse(is.na(st_helena$phen_stage_cab), 0, st_helena$PRCP)
    st_hosp$prcpbase_cab <- ifelse(is.na(st_hosp$phen_stage_cab), 0, st_hosp$PRCP)
    #merlot
    st_helena$prcpbase_mer <- ifelse(is.na(st_helena$phen_stage_mer), 0, st_helena$PRCP)
    st_hosp$prcpbase_mer <- ifelse(is.na(st_hosp$phen_stage_mer), 0, st_hosp$PRCP)
    
  #Summation by phenological stage and variety
    #general
    st_helena$prcpsum_phen <- ave(st_helena$prcpbase_gen, by = list(st_helena$year, st_helena$phen_stage), FUN = cumsum)
    st_hosp$prcpsum_phen <- ave(st_hosp$prcpbase_gen, by = list(st_hosp$year, st_hosp$phen_stage), FUN = cumsum)
    #merlot
    st_helena$prcpsum_phen_mer <- ave(st_helena$prcpbase_mer, by = list(st_helena$year, st_helena$phen_stage_mer), FUN = cumsum)
    st_hosp$prcpsum_phen_mer <- ave(st_hosp$prcpbase_mer, by = list(st_hosp$year, st_hosp$phen_stage_mer), FUN = cumsum)
    #chardonnay
    st_helena$prcpsum_phen_chard <- ave(st_helena$prcpbase_chard, by = list(st_helena$year, st_helena$phen_stage_chard), FUN = cumsum)
    st_hosp$prcpsum_phen_chard <- ave(st_hosp$prcpbase_chard, by = list(st_hosp$year, st_hosp$phen_stage_chard), FUN = cumsum)
    #cabernet
    st_helena$prcpsum_phen_cab <- ave(st_helena$prcpbase_cab, by = list(st_helena$year, st_helena$phen_stage_cab), FUN = cumsum)
    st_hosp$prcpsum_phen_cab <- ave(st_hosp$prcpbase_cab, by = list(st_hosp$year, st_hosp$phen_stage_cab), FUN = cumsum)
    
  #Summation by year and variety
    #general
    st_helena$prcpsum_year <- ave(st_helena$prcpbase_gen, st_helena$year, FUN = cumsum)
    st_hosp$prcpsum_year <- ave(st_hosp$prcpbase_gen, st_hosp$year, FUN = cumsum)
    #merlot
    st_helena$prcpsum_year_mer <- ave(st_helena$prcpbase_mer, st_helena$year, FUN = cumsum)
    st_hosp$prcpsum_year_mer <- ave(st_hosp$prcpbase_mer, st_hosp$year, FUN = cumsum)
    #chardonnay
    st_helena$prcpsum_year_chard <- ave(st_helena$prcpbase_chard, st_helena$year, FUN = cumsum)
    st_hosp$prcpsum_year_chard <- ave(st_hosp$prcpbase_chard, st_hosp$year, FUN = cumsum)
    #cabernet
    st_helena$prcpsum_year_cab <- ave(st_helena$prcpbase_cab, st_helena$year, FUN = cumsum)
    st_hosp$prcpsum_year_cab <- ave(st_hosp$prcpbase_cab, st_hosp$year, FUN = cumsum)
    
#Creating dataframe of agg precipitation from each year and phen stage and each location
    
  #By Phen Stage
    #general
    agp_st_helena_phen_gen <- aggregate(st_helena$prcpbase_gen, by = list(st_helena$year, st_helena$phen_stage), FUN=sum) 
    colnames(agp_st_helena_phen_gen)[1] <- c("Vintage")
    colnames(agp_st_helena_phen_gen)[2] <- c("phen_stage")
    colnames(agp_st_helena_phen_gen)[3] <- c("st_helena_prcp")
    
    agp_st_hosp_phen_gen <- aggregate(st_hosp$prcpbase_gen, by = list(st_hosp$year, st_hosp$phen_stage), FUN=sum) 
    colnames(agp_st_hosp_phen_gen)[1] <- c("Vintage")
    colnames(agp_st_hosp_phen_gen)[2] <- c("phen_stage")
    colnames(agp_st_hosp_phen_gen)[3] <- c("st_hosp_prcp")
    
    #cabernet
    agp_st_helena_phen_cab <- aggregate(st_helena$prcpbase_cab, by = list(st_helena$year, st_helena$phen_stage_cab), FUN=sum) 
    colnames(agp_st_helena_phen_cab)[1] <- c("Vintage")
    colnames(agp_st_helena_phen_cab)[2] <- c("phen_stage")
    colnames(agp_st_helena_phen_cab)[3] <- c("st_helena_prcp_cab")
    
    agp_st_hosp_phen_cab <- aggregate(st_hosp$prcpbase_cab, by = list(st_hosp$year, st_hosp$phen_stage_cab), FUN=sum) 
    colnames(agp_st_hosp_phen_cab)[1] <- c("Vintage")
    colnames(agp_st_hosp_phen_cab)[2] <- c("phen_stage")
    colnames(agp_st_hosp_phen_cab)[3] <- c("st_hosp_prcp_cab")
    
    #chardonnay
    agp_st_helena_phen_chard <- aggregate(st_helena$prcpbase_chard, by = list(st_helena$year, st_helena$phen_stage_chard), FUN=sum) 
    colnames(agp_st_helena_phen_chard)[1] <- c("Vintage")
    colnames(agp_st_helena_phen_chard)[2] <- c("phen_stage")
    colnames(agp_st_helena_phen_chard)[3] <- c("st_helena_prcp_chard")
    
    agp_st_hosp_phen_chard <- aggregate(st_hosp$prcpbase_chard, by = list(st_hosp$year, st_hosp$phen_stage_chard), FUN=sum) 
    colnames(agp_st_hosp_phen_chard)[1] <- c("Vintage")
    colnames(agp_st_hosp_phen_chard)[2] <- c("phen_stage")
    colnames(agp_st_hosp_phen_chard)[3] <- c("st_hosp_prcp_chard")
    
    #merlot
    agp_st_helena_phen_mer <- aggregate(st_helena$prcpbase_mer, by = list(st_helena$year, st_helena$phen_stage), FUN=sum) 
    colnames(agp_st_helena_phen_mer)[1] <- c("Vintage")
    colnames(agp_st_helena_phen_mer)[2] <- c("phen_stage")
    colnames(agp_st_helena_phen_mer)[3] <- c("st_helena_prcp_mer")
    
    agp_st_hosp_phen_mer <- aggregate(st_hosp$prcpbase_mer, by = list(st_hosp$year, st_hosp$phen_stage_mer), FUN=sum) 
    colnames(agp_st_hosp_phen_mer)[1] <- c("Vintage")
    colnames(agp_st_hosp_phen_mer)[2] <- c("phen_stage")
    colnames(agp_st_hosp_phen_mer)[3] <- c("st_hosp_prcp_mer")
    
    prcp_agg_phen <- merge(agp_st_helena_phen_gen, agp_st_hosp_phen_gen)
    prcp_agg_phen <- merge(prcp_agg_phen, agp_st_hosp_phen_cab)
    prcp_agg_phen <- merge(prcp_agg_phen, agp_st_helena_phen_cab)
    prcp_agg_phen <- merge(prcp_agg_phen, agp_st_hosp_phen_chard)
    prcp_agg_phen <- merge(prcp_agg_phen, agp_st_helena_phen_chard)
    prcp_agg_phen <- merge(prcp_agg_phen, agp_st_hosp_phen_mer)
    prcp_agg_phen <- merge(prcp_agg_phen, agp_st_helena_phen_mer)
    
  #averaging by location for each variety
    prcp_agg_phen$prcp_avg_phen_gen <- rowMeans(prcp_agg_phen[, 3:4])
    prcp_agg_phen$prcp_avg_phen_cab <- rowMeans(prcp_agg_phen[, 5:6])
    prcp_agg_phen$prcp_avg_phen_chard <- rowMeans(prcp_agg_phen[, 7:8])
    prcp_agg_phen$prcp_avg_phen_mer <- rowMeans(prcp_agg_phen[, 9:10])
    
  #By Year
    #gen
    agp_st_helena_year_gen <- aggregate(st_helena$prcpbase_gen, by = list(st_helena$year), FUN=sum) 
    colnames(agp_st_helena_year_gen)[1] <- c("Vintage")
    colnames(agp_st_helena_year_gen)[2] <- c("st_helena_prcp")
    
    agp_st_hosp_year_gen <- aggregate(st_hosp$prcpbase_gen, by = list(st_hosp$year), FUN=sum) 
    colnames(agp_st_hosp_year_gen)[1] <- c("Vintage")
    colnames(agp_st_hosp_year_gen)[2] <- c("st_hosp_prcp")
    
    #cabernet
    agp_st_helena_year_cab <- aggregate(st_helena$prcpbase_cab, by = list(st_helena$year), FUN=sum) 
    colnames(agp_st_helena_year_cab)[1] <- c("Vintage")
    colnames(agp_st_helena_year_cab)[2] <- c("st_helena_prcp_cab")
    
    agp_st_hosp_year_cab <- aggregate(st_hosp$prcpbase_cab, by = list(st_hosp$year), FUN=sum) 
    colnames(agp_st_hosp_year_cab)[1] <- c("Vintage")
    colnames(agp_st_hosp_year_cab)[2] <- c("st_hosp_prcp_cab")
    
    #chardonnay
    agp_st_helena_year_chard <- aggregate(st_helena$prcpbase_chard, by = list(st_helena$year), FUN=sum) 
    colnames(agp_st_helena_year_chard)[1] <- c("Vintage")
    colnames(agp_st_helena_year_chard)[2] <- c("st_helena_prcp_chard")
    
    agp_st_hosp_year_chard <- aggregate(st_hosp$prcpbase_chard, by = list(st_hosp$year), FUN=sum) 
    colnames(agp_st_hosp_year_chard)[1] <- c("Vintage")
    colnames(agp_st_hosp_year_chard)[2] <- c("st_hosp_prcp_chard")
    
    #merlot
    agp_st_helena_year_mer <- aggregate(st_helena$prcpbase_mer, by = list(st_helena$year), FUN=sum) 
    colnames(agp_st_helena_year_mer)[1] <- c("Vintage")
    colnames(agp_st_helena_year_mer)[2] <- c("st_helena_prcp_mer")
    
    agp_st_hosp_year_mer <- aggregate(st_hosp$prcpbase_mer, by = list(st_hosp$year), FUN=sum) 
    colnames(agp_st_hosp_year_mer)[1] <- c("Vintage")
    colnames(agp_st_hosp_year_mer)[2] <- c("st_hosp_prcp_mer")
    
    prcp_agg_year <- merge(agp_st_helena_year_gen, agp_st_hosp_year_gen)
    prcp_agg_year <- merge(prcp_agg_year, agp_st_helena_year_cab)
    prcp_agg_year <- merge(prcp_agg_year, agp_st_hosp_year_cab)
    prcp_agg_year <- merge(prcp_agg_year, agp_st_helena_year_chard)
    prcp_agg_year <- merge(prcp_agg_year, agp_st_hosp_year_chard)
    prcp_agg_year <- merge(prcp_agg_year, agp_st_helena_year_mer)
    prcp_agg_year <- merge(prcp_agg_year, agp_st_hosp_year_mer)
    
    #averaging by location for each variety
    prcp_agg_year$prcp_avg_year_gen <- rowMeans(prcp_agg_year[, 2:3])
    prcp_agg_year$prcp_avg_year_cab <- rowMeans(prcp_agg_year[, 4:5])
    prcp_agg_year$prcp_avg_year_chard <- rowMeans(prcp_agg_year[, 6:7])
    prcp_agg_year$prcp_avg_year_mer <- rowMeans(prcp_agg_year[, 8:9])
  
#Subsetting by location (Napa/Sonoma/North Coast)
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
  
  nc_cabernet <- subset(north_coast, north_coast$Variety=="Cabernet" | north_coast$Variety=="Cabernet ")
  nc_chardonnay <- subset(north_coast, north_coast$Variety=="Chardonnay")
  nc_zinfandel <- subset(north_coast, north_coast$Variety=="Zinfandel")

#creating 2 dataframes. One by year, one by phen. Both with gdd_agg, prcp_agg, vintage, and napa ratings
  Agg_Table_year <- merge(prcp_agg_year, gdd_agg_year)
  Agg_Table_phen <- merge(prcp_agg_phen, gdd_agg_phen)
  
  #napa - year
  Cabernet_Table_N <- merge(Agg_Table_year, napa_cabernet)
    Cabernet_Table_N <- Cabernet_Table_N[,-c(2,3,4,5,6,7,8,9,10,12,13,14,15,16,17,18,19,20,21,22,24,25)] #keeping averages
    colnames(Cabernet_Table_N)[3] <- c("var_gdd_avg_year")
    colnames(Cabernet_Table_N)[2] <- c("var_prcp_avg_year")
  
  Chardonnay_Table_N <- merge(Agg_Table_year, napa_chardonnay)
    Chardonnay_Table_N <- Chardonnay_Table_N[,-c(2,3,4,5,6,7,8,9,10,11,13,14,15,16,17,18,19,20,21,22,23,25)] #keeping averages
    colnames(Chardonnay_Table_N)[2] <- c("var_prcp_avg_year")
    colnames(Chardonnay_Table_N)[3] <- c("var_gdd_avg_year")
  
  Merlot_Table_N <- merge(Agg_Table_year, napa_merlot)
    Merlot_Table_N <- Merlot_Table_N[,-c(2,3,4,5,6,7,8,9,10,11,12,14,15,16,17,18,19,20,21,22,23,24)] #keeping averages
    colnames(Merlot_Table_N)[2] <- c("var_prcp_avg_year")
    colnames(Merlot_Table_N)[3] <- c("var_gdd_avg_year")
  
  Rhone_Table_N <- merge(Agg_Table_year, napa_rhone) #values from gen
    Rhone_Table_N <- Rhone_Table_N[,-c(2,3,4,5,6,7,8,9,11,12,13,14,15,16,17,18,19,20,21,23,24,25)] #keeping averages
    colnames(Rhone_Table_N)[2] <- c("var_prcp_avg_year")
    colnames(Rhone_Table_N)[3] <- c("var_gdd_avg_year")
  
  Zinfandel_Table_N <- merge(Agg_Table_year, napa_zinfandel) #values from gen
    Zinfandel_Table_N <- Zinfandel_Table_N[,-c(2,3,4,5,6,7,8,9,11,12,13,14,15,16,17,18,19,20,21,23,24,25)] #keeping averages
    colnames(Zinfandel_Table_N)[2] <- c("var_prcp_avg_year")
    colnames(Zinfandel_Table_N)[3] <- c("var_gdd_avg_year")
  
  Napa_Table_Full_Year <- rbind(Cabernet_Table_N, Chardonnay_Table_N, Merlot_Table_N, Rhone_Table_N, Zinfandel_Table_N)
  Napa_Table_Full_Year <- Napa_Table_Full_Year[,c(1,4,5,6,7,8,9,2,3,10,11)] #reorganize columns
  
  #napa phen
  Cabernet_Table_N <- merge(Agg_Table_phen, napa_cabernet)
    Cabernet_Table_N <- Cabernet_Table_N[,-c(3,4,5,6,7,8,9,10,11,13,14,15,16,17,18,19,20,21,22,23,25,26)] #keeping averages
    colnames(Cabernet_Table_N)[4] <- c("var_gdd_avg_phen")
    colnames(Cabernet_Table_N)[3] <- c("var_prcp_avg_phen")
  
  Chardonnay_Table_N <- merge(Agg_Table_phen, napa_chardonnay)
    Chardonnay_Table_N <- Chardonnay_Table_N[,-c(3,4,5,6,7,8,9,10,11,12,14,15,16,17,18,19,20,21,22,23,24,26)] #keeping averages
    colnames(Chardonnay_Table_N)[4] <- c("var_gdd_avg_phen")
    colnames(Chardonnay_Table_N)[3] <- c("var_prcp_avg_phen")
  
  Merlot_Table_N <- merge(Agg_Table_phen, napa_merlot)
    Merlot_Table_N <- Merlot_Table_N[,-c(3,4,5,6,7,8,9,10,11,12,13,15,16,17,18,19,20,21,22,23,24,25)] #keeping averages
    colnames(Merlot_Table_N)[3] <- c("var_prcp_avg_phen")
    colnames(Merlot_Table_N)[4] <- c("var_gdd_avg_phen")
  
  Rhone_Table_N <- merge(Agg_Table_phen, napa_rhone)
    Rhone_Table_N <- Rhone_Table_N[,-c(3,4,5,6,7,8,9,10,12,13,14,15,16,17,18,19,20,21,22,24,25,26)] #keeping averages
    colnames(Rhone_Table_N)[4] <- c("var_gdd_avg_phen")
    colnames(Rhone_Table_N)[3] <- c("var_prcp_avg_phen")
  
  Zinfandel_Table_N <- merge(Agg_Table_phen, napa_zinfandel)
    Zinfandel_Table_N <- Zinfandel_Table_N[,-c(3,4,5,6,7,8,9,10,12,13,14,15,16,17,18,19,20,21,22,24,25,26)] #keeping averages
    colnames(Zinfandel_Table_N)[4] <- c("var_gdd_avg_phen")
    colnames(Zinfandel_Table_N)[3] <- c("var_prcp_avg_phen")
  
  Napa_Table_Full_Phen <- rbind(Cabernet_Table_N, Chardonnay_Table_N, Merlot_Table_N, Rhone_Table_N, Zinfandel_Table_N)
  Napa_Table_Full_Phen <- pivot_wider(Napa_Table_Full_Phen, names_from = phen_stage, values_from = c(var_prcp_avg_phen, var_gdd_avg_phen))
  Napa_Table_Full_Phen <- Napa_Table_Full_Phen[,c(1,2,3,4,5,6,7,11,12,10,14,15,13,8,9)] #reorganize columns
  
  
  #sonoma - year
  Cabernet_Table_S <- merge(Agg_Table_year, sonoma_cabernet)
    Cabernet_Table_S <- Cabernet_Table_S[,-c(2,3,4,5,6,7,8,9,10,12,13,14,15,16,17,18,19,20,21,22,24,25)] #keeping averages
    colnames(Cabernet_Table_S)[3] <- c("var_gdd_avg_year")
    colnames(Cabernet_Table_S)[2] <- c("var_prcp_avg_year")
  
  Chardonnay_Table_S <- merge(Agg_Table_year, sonoma_chardonnay)
    Chardonnay_Table_S <- Chardonnay_Table_S[,-c(2,3,4,5,6,7,8,9,10,11,13,14,15,16,17,18,19,20,21,22,23,25)] #keeping averages
    colnames(Chardonnay_Table_S)[2] <- c("var_prcp_avg_year")
    colnames(Chardonnay_Table_S)[3] <- c("var_gdd_avg_year")
  
  Merlot_Table_S <- merge(Agg_Table_year, sonoma_merlot)
    Merlot_Table_S <- Merlot_Table_S[,-c(2,3,4,5,6,7,8,9,10,11,12,14,15,16,17,18,19,20,21,22,23,24)] #keeping averages
    colnames(Merlot_Table_S)[2] <- c("var_prcp_avg_year")
    colnames(Merlot_Table_S)[3] <- c("var_gdd_avg_year")
  
  Rhone_Table_S <- merge(Agg_Table_year, sonoma_rhone) #values from gen
    Rhone_Table_S <- Rhone_Table_S[,-c(2,3,4,5,6,7,8,9,11,12,13,14,15,16,17,18,19,20,21,23,24,25)] #keeping averages
    colnames(Rhone_Table_S)[2] <- c("var_prcp_avg_year")
    colnames(Rhone_Table_S)[3] <- c("var_gdd_avg_year")
  
  Zinfandel_Table_S <- merge(Agg_Table_year, sonoma_zinfandel) #values from gen
    Zinfandel_Table_S <- Zinfandel_Table_S[,-c(2,3,4,5,6,7,8,9,11,12,13,14,15,16,17,18,19,20,21,23,24,25)] #keeping averages
    colnames(Zinfandel_Table_S)[2] <- c("var_prcp_avg_year")
    colnames(Zinfandel_Table_S)[3] <- c("var_gdd_avg_year")
  
  Sonoma_Table_Full_Year <- rbind(Cabernet_Table_S, Chardonnay_Table_S, Merlot_Table_S, Rhone_Table_S, Zinfandel_Table_S)
  Sonoma_Table_Full_Year <- Sonoma_Table_Full_Year[,c(1,4,5,6,7,8,9,2,3,10,11)] #reorganize columns
  
  #sonoma - phen
  Cabernet_Table_S <- merge(Agg_Table_phen, sonoma_cabernet)
    Cabernet_Table_S <- Cabernet_Table_S[,-c(3,4,5,6,7,8,9,10,11,13,14,15,16,17,18,19,20,21,22,23,25,26)] #keeping averages
    colnames(Cabernet_Table_S)[4] <- c("var_gdd_avg_phen")
    colnames(Cabernet_Table_S)[3] <- c("var_prcp_avg_phen")
  
  Chardonnay_Table_S <- merge(Agg_Table_phen, sonoma_chardonnay)
    Chardonnay_Table_S <- Chardonnay_Table_S[,-c(3,4,5,6,7,8,9,10,11,12,14,15,16,17,18,19,20,21,22,23,24,26)] #keeping averages
    colnames(Chardonnay_Table_S)[4] <- c("var_gdd_avg_phen")
    colnames(Chardonnay_Table_S)[3] <- c("var_prcp_avg_phen")
  
  Merlot_Table_S <- merge(Agg_Table_phen, sonoma_merlot)
    Merlot_Table_S <- Merlot_Table_S[,-c(3,4,5,6,7,8,9,10,11,12,13,15,16,17,18,19,20,21,22,23,24,25)] #keeping averages
    colnames(Merlot_Table_S)[3] <- c("var_prcp_avg_phen")
    colnames(Merlot_Table_S)[4] <- c("var_gdd_avg_phen")
  
  Rhone_Table_S <- merge(Agg_Table_phen, sonoma_rhone)
    Rhone_Table_S <- Rhone_Table_S[,-c(3,4,5,6,7,8,9,10,12,13,14,15,16,17,18,19,20,21,22,24,25,26)] #keeping averages
    colnames(Rhone_Table_S)[4] <- c("var_gdd_avg_phen")
    colnames(Rhone_Table_S)[3] <- c("var_prcp_avg_phen")
  
  Zinfandel_Table_S <- merge(Agg_Table_phen, sonoma_zinfandel)
    Zinfandel_Table_S <- Zinfandel_Table_S[,-c(3,4,5,6,7,8,9,10,12,13,14,15,16,17,18,19,20,21,22,24,25,26)] #keeping averages
    colnames(Zinfandel_Table_S)[4] <- c("var_gdd_avg_phen")
    colnames(Zinfandel_Table_S)[3] <- c("var_prcp_avg_phen")
  
  Sonoma_Table_Full_Phen <- rbind(Cabernet_Table_S, Chardonnay_Table_S, Merlot_Table_S, Rhone_Table_S, Zinfandel_Table_S)
  Sonoma_Table_Full_Phen <- pivot_wider(Sonoma_Table_Full_Phen, names_from = phen_stage, values_from = c(var_prcp_avg_phen, var_gdd_avg_phen))
  Sonoma_Table_Full_Phen <- Sonoma_Table_Full_Phen[,c(1,2,3,4,5,6,7,10,11,12,13,14,15,8,9)] #reorganize columns
  
  #north coast - year
  Cabernet_Table_NC <- merge(Agg_Table_year, nc_cabernet)
    Cabernet_Table_NC <- Cabernet_Table_NC[,-c(2,3,4,5,6,7,8,9,10,12,13,14,15,16,17,18,19,20,21,22,24,25)] #keeping averages
    colnames(Cabernet_Table_NC)[3] <- c("var_gdd_avg_year")
    colnames(Cabernet_Table_NC)[2] <- c("var_prcp_avg_year")
  
  Chardonnay_Table_NC <- merge(Agg_Table_year, nc_chardonnay)
    Chardonnay_Table_NC <- Chardonnay_Table_NC[,-c(2,3,4,5,6,7,8,9,10,11,13,14,15,16,17,18,19,20,21,22,23,25)] #keeping averages
    colnames(Chardonnay_Table_NC)[2] <- c("var_prcp_avg_year")
    colnames(Chardonnay_Table_NC)[3] <- c("var_gdd_avg_year")
  
  Zinfandel_Table_NC <- merge(Agg_Table_year, nc_zinfandel) #values from gen
    Zinfandel_Table_NC <- Zinfandel_Table_NC[,-c(2,3,4,5,6,7,8,9,11,12,13,14,15,16,17,18,19,20,21,23,24,25)] #keeping averages
    colnames(Zinfandel_Table_NC)[2] <- c("var_prcp_avg_year")
    colnames(Zinfandel_Table_NC)[3] <- c("var_gdd_avg_year")
  
  NC_Table_Full_Year <- rbind(Cabernet_Table_NC, Chardonnay_Table_NC, Zinfandel_Table_NC)
  NC_Table_Full_Year <- NC_Table_Full_Year[,c(1,4,5,6,7,8,9,2,3,10,11)] #reorganize columns
  
  #north coast - phen
  Cabernet_Table_NC <- merge(Agg_Table_phen, nc_cabernet)
    Cabernet_Table_NC <- Cabernet_Table_NC[,-c(3,4,5,6,7,8,9,10,11,13,14,15,16,17,18,19,20,21,22,23,25,26)] #keeping averages
    colnames(Cabernet_Table_NC)[4] <- c("var_gdd_avg_phen")
    colnames(Cabernet_Table_NC)[3] <- c("var_prcp_avg_phen")
  
  Chardonnay_Table_NC <- merge(Agg_Table_phen, nc_chardonnay)
    Chardonnay_Table_NC <- Chardonnay_Table_NC[,-c(3,4,5,6,7,8,9,10,11,12,14,15,16,17,18,19,20,21,22,23,24,26)] #keeping averages
    colnames(Chardonnay_Table_NC)[4] <- c("var_gdd_avg_phen")
    colnames(Chardonnay_Table_NC)[3] <- c("var_prcp_avg_phen")
  
  Zinfandel_Table_NC <- merge(Agg_Table_phen, nc_zinfandel)
    Zinfandel_Table_NC <- Zinfandel_Table_NC[,-c(3,4,5,6,7,8,9,10,12,13,14,15,16,17,18,19,20,21,22,24,25,26)] #keeping averages
    colnames(Zinfandel_Table_NC)[4] <- c("var_gdd_avg_phen")
    colnames(Zinfandel_Table_NC)[3] <- c("var_prcp_avg_phen")
  
  NC_Table_Full_Phen <- rbind(Cabernet_Table_NC, Chardonnay_Table_NC, Zinfandel_Table_NC)
  NC_Table_Full_Phen <- pivot_wider(NC_Table_Full_Phen, names_from = phen_stage, values_from = c(var_prcp_avg_phen, var_gdd_avg_phen))
  NC_Table_Full_Phen <- NC_Table_Full_Phen[,c(1,2,3,4,5,6,7,11,10,12,14,13,15,8,9)] #reorganize columns
  
  #Exporting as csv for future modeling
   write.csv(Napa_Table_Full_Year,"/Users/phoebeautio/Desktop/Vintage Research/TablesForModels/NapaComplete_year.csv", row.names = FALSE)
   write.csv(Sonoma_Table_Full_Year,"/Users/phoebeautio/Desktop/Vintage Research/TablesForModels/SonomaComplete_year.csv", row.names = FALSE)
   write.csv(NC_Table_Full_Year,"/Users/phoebeautio/Desktop/Vintage Research/TablesForModels/NorthCoastComplete_year.csv", row.names = FALSE)

   write.csv(Napa_Table_Full_Phen,"/Users/phoebeautio/Desktop/Vintage Research/TablesForModels/NapaComplete_phen.csv", row.names = FALSE)
   write.csv(Sonoma_Table_Full_Phen,"/Users/phoebeautio/Desktop/Vintage Research/TablesForModels/SonomaComplete_phen.csv", row.names = FALSE)
   write.csv(NC_Table_Full_Phen,"/Users/phoebeautio/Desktop/Vintage Research/TablesForModels/NorthCoastComplete_phen.csv", row.names = FALSE)
  
