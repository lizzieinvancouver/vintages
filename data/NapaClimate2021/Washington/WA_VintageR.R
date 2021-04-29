################# Washington Vintage Dataset (PA) - Updated 4/27/2021 #########################

#Housekeeping
rm(list=ls())
options(stringsAsFactors = FALSE)

#Load libraries
library(lubridate)
library(dplyr)
library(tidyr)

#Reading in csv files
mydat <- read.csv("/Users/phoebeautio/Desktop/Vintages/Data/NapaClimate2021/Washington/WA_Vintage.csv", header=TRUE, na.strings=c(""," ","NA"))
head(mydat)

dayt <- read.csv("/Users/phoebeautio/Desktop/Vintages/data/NapaClimate2021/Washington/DaytClean.csv", header=TRUE, na.strings=c(""," ","NA"))
head(dayt)

hat <- read.csv("/Users/phoebeautio/Desktop/Vintages/data/NapaClimate2021/Washington/HatClean.csv", header=TRUE, na.strings=c(""," ","NA"))
head(hat)

## Relative paths (Geoff)
## mydat <- read.csv("OR_Vintage.csv", header=TRUE, na.strings=c(""," ","NA"))
## head(mydat)

#Adding Phenology Column (by Variety)
dayt['phen_stage_cab'] <- NA
dayt['phen_stage_syrah'] <- NA
dayt['phen_stage_mer'] <- NA

hat['phen_stage_cab'] <- NA
hat['phen_stage_syrah'] <- NA
hat['phen_stage_mer'] <- NA

#syrah (general)
#Budburst (1)
dayt$phen_stage_syrah[which(dayt$month=="4")] <- "1"
dayt$phen_stage_syrah[which(dayt$month=="5" & dayt$day %in% c("7","8","9","10","11","12","13","14","15"))] <- "1"

hat$phen_stage_syrah[which(hat$month=="4")] <- "1"
hat$phen_stage_syrah[which(hat$month=="5" & hat$day %in% c("1","2","3","4","5","6","7","8","9","10","11","12","13","14","15"))] <- "1"

#Bloom (2)
dayt$phen_stage_syrah[which(dayt$month=="5" & dayt$day %in% c("16","17","18","19","20","21","22",
                                                              "23","24","25","26","27","28","29","30","31"))] <- "2"
dayt$phen_stage_syrah[which(dayt$month=="6")] <- "2"
dayt$phen_stage_syrah[which(dayt$month=="7" & dayt$day %in% c("1","2","3","4","5","6","7","8","9",
                                                              "10","11","12","13","14","15","16",
                                                              "17","18","19","20","21","22","23"))] <- "2"
hat$phen_stage_syrah[which(hat$month=="5" & hat$day %in% c("16","17","18","19","20","21","22",
                                                            "23","24","25","26","27","28","29","30","31"))] <- "2"
hat$phen_stage_syrah[which(hat$month=="6")] <- "2"
hat$phen_stage_syrah[which(hat$month=="7" & hat$day %in% c("1","2","3","4","5","6","7","8","9",
                                                            "10","11","12","13","14","15","16",
                                                            "17","18","19","20","21","22","23"))] <- "2"
#Veraison (3)
dayt$phen_stage_syrah[which(dayt$month=="7" & dayt$day %in% c("24","25","26","27","28","29","30","31"))] <- "3"
dayt$phen_stage_syrah[which(dayt$month=="8")] <- "3"
dayt$phen_stage_syrah[which(dayt$month=="9" & dayt$day %in% c("1","2","3","4","5","6","7","8","9",
                                                              "10","11","12","13","14","15"))] <- "3"

hat$phen_stage_syrah[which(hat$month=="7" & hat$day %in% c("24","25","26","27","28","29","30","31"))] <- "3"
hat$phen_stage_syrah[which(hat$month=="8")] <- "3"
hat$phen_stage_syrah[which(hat$month=="9" & hat$day %in% c("1","2","3","4","5","6","7","8","9",
                                                            "10","11","12","13","14","15"))] <- "3"

#Cabernet (general)
#Budburst (1)
dayt$phen_stage_cab[which(dayt$month=="3" & dayt$day %in% c("25","26","27","28","29","30","31"))] <- "1"
dayt$phen_stage_cab[which(dayt$month=="4")] <- "1"
dayt$phen_stage_cab[which(dayt$month=="5" & dayt$day %in% c("1","2","3","4","5","6","7","8","9","10","11","12","13","14","15"))] <- "1"

hat$phen_stage_cab[which(hat$month=="3" & hat$day %in% c("25","26","27","28","29","30","31"))] <- "1"
hat$phen_stage_cab[which(hat$month=="4")] <- "1"
hat$phen_stage_cab[which(hat$month=="5" & hat$day %in% c("1","2","3","4","5","6","7","8","9","10","11","12","13","14","15"))] <- "1"

#Bloom (2)
dayt$phen_stage_cab[which(dayt$month=="5" & dayt$day %in% c("16","17","18","19","20","21","22",
                                                                     "23","24","25","26","27","28","29","30","31"))] <- "2"
dayt$phen_stage_cab[which(dayt$month=="6")] <- "2"
dayt$phen_stage_cab[which(dayt$month=="7" & dayt$day %in% c("1","2","3","4","5","6","7","8","9",
                                                                     "10","11","12","13","14","15","16",
                                                                     "17","18","19","20","21","22","23"))] <- "2"


hat$phen_stage_cab[which(hat$month=="5" & hat$day %in% c("16","17","18","19","20","21","22",
                                                                           "23","24","25","26","27","28","29","30","31"))] <- "2"
hat$phen_stage_cab[which(hat$month=="6")] <- "2"
hat$phen_stage_cab[which(hat$month=="7" & hat$day %in% c("1","2","3","4","5","6","7","8","9",
                                                                           "10","11","12","13","14","15","16",
                                                                           "17","18","19","20","21","22","23"))] <- "2"
#Veraison (3)
dayt$phen_stage_cab[which(dayt$month=="7" & dayt$day %in% c("23","24","25","26","27","28","29","30","31"))] <- "3"
dayt$phen_stage_cab[which(dayt$month=="8")] <- "3"
dayt$phen_stage_cab[which(dayt$month=="9" & dayt$day %in% c("1","2","3","4","5","6","7","8","9",
                                                                     "10","11","12","13","14","15","16",
                                                                     "17","18","19","20","21","22","23"))] <- "3"

hat$phen_stage_cab[which(hat$month=="7" & hat$day %in% c("23","24","25","26","27","28","29","30","31"))] <- "3"
hat$phen_stage_cab[which(hat$month=="8")] <- "3"
hat$phen_stage_cab[which(hat$month=="9" & hat$day %in% c("1","2","3","4","5","6","7","8","9",
                                                                           "10","11","12","13","14","15","16",
                                                                           "17","18","19","20","21","22","23"))] <- "3"
#Merlot
#Budburst (1)
dayt$phen_stage_mer[which(dayt$month=="3" & dayt$day %in% c("25","26","27","28","29","30","31"))] <- "1"
dayt$phen_stage_mer[which(dayt$month=="4")] <- "1"
dayt$phen_stage_mer[which(dayt$month=="5" & dayt$day %in% c("1","2","3","4","5","6","7","8","9","10","11","12","13"))] <- "1"

hat$phen_stage_mer[which(hat$month=="3" & hat$day %in% c("25","26","27","28","29","30","31"))] <- "1"
hat$phen_stage_mer[which(hat$month=="4")] <- "1"
hat$phen_stage_mer[which(hat$month=="5" & hat$day %in% c("1","2","3","4","5","6","7","8","9","10","11","12","13"))] <- "1"

#Bloom (2)
dayt$phen_stage_mer[which(dayt$month=="5" & dayt$day %in% c("14","15","16","17","18","19","20","21","22",
                                                                     "23","24","25","26","27","28","29","30","31"))] <- "2"
dayt$phen_stage_mer[which(dayt$month=="6")] <- "2"
dayt$phen_stage_mer[which(dayt$month=="7" & dayt$day %in% c("1","2","3","4","5","6","7","8","9",
                                                                     "10","11","12","13","14","15"))] <- "2"


hat$phen_stage_mer[which(hat$month=="5" & hat$day %in% c("14","15","16","17","18","19","20","21","22",
                                                                           "23","24","25","26","27","28","29","30","31"))] <- "2"
hat$phen_stage_mer[which(hat$month=="6")] <- "2"
hat$phen_stage_mer[which(hat$month=="7" & hat$day %in% c("1","2","3","4","5","6","7","8","9",
                                                                           "10","11","12","13","14","15"))] <- "2"
#Veraison (3)
dayt$phen_stage_mer[which(dayt$month=="7" & dayt$day %in% c("16","17","18","19","20","21","22","23","24","25",
                                                                     "26","27","28","29","30","31"))] <- "3"
dayt$phen_stage_mer[which(dayt$month=="8")] <- "3"
dayt$phen_stage_mer[which(dayt$month=="9" & dayt$day %in% c("1","2","3","4","5","6","7","8","9",
                                                                     "10","11","12","13","14","15"))] <- "3"

hat$phen_stage_mer[which(hat$month=="7" & hat$day %in% c("16","17","18","19","20","21","22","23","24","25",
                                                                           "26","27","28","29","30","31"))] <- "3"
hat$phen_stage_mer[which(hat$month=="8")] <- "3"
hat$phen_stage_mer[which(hat$month=="9" & hat$day %in% c("1","2","3","4","5","6","7","8","9",
                                                                           "10","11","12","13","14","15"))] <- "3"
#remove empty rows
dayt <- dayt[!(is.na(dayt$phen_stage_cab) & is.na(dayt$phen_stage_syrah) & is.na(dayt$phen_stage_mer)), ]
hat <- hat[!(is.na(hat$phen_stage_cab) & is.na(hat$phen_stage_syrah) & is.na(hat$phen_stage_mer)), ]

#Calculating GDD at each location -- GDD base temp = 10
#Base GDD values - need individual per variety
#syrah
hat$gddbase_syrah <- ifelse(!is.na(hat$phen_stage_syrah) & hat$TAVG >= 10, hat$TAVG - 10, 0)
dayt$gddbase_syrah <- ifelse(!is.na(dayt$phen_stage_syrah) & dayt$TAVG >= 10, dayt$TAVG - 10, 0)
#cabernet
hat$gddbase_cab <- ifelse(!is.na(hat$phen_stage_cab) & hat$TAVG >= 10, hat$TAVG - 10, 0)
dayt$gddbase_cab <- ifelse(!is.na(dayt$phen_stage_cab) & dayt$TAVG >= 10, dayt$TAVG - 10, 0)
#merlot
hat$gddbase_mer <- ifelse(!is.na(hat$phen_stage_mer) & hat$TAVG >= 10, hat$TAVG - 10, 0)
dayt$gddbase_mer <- ifelse(!is.na(dayt$phen_stage_mer) & dayt$TAVG >= 10, dayt$TAVG - 10, 0)

#Summation by phenological stage and variety
#merlot
hat$gdd_phen_mer <- ave(hat$gddbase_mer, by = list(hat$year, hat$phen_stage_mer), FUN = cumsum)
dayt$gdd_phen_mer <- ave(dayt$gddbase_mer, by = list(dayt$year, dayt$phen_stage_mer), FUN = cumsum)
#syrah
hat$gdd_phen_syrah <- ave(hat$gddbase_syrah, by = list(hat$year, hat$phen_stage_syrah), FUN = cumsum)
dayt$gdd_phen_syrah <- ave(dayt$gddbase_syrah, by = list(dayt$year, dayt$phen_stage_syrah), FUN = cumsum)
#cabernet
hat$gdd_phen_cab <- ave(hat$gddbase_cab, by = list(hat$year, hat$phen_stage_cab), FUN = cumsum)
dayt$gdd_phen_cab <- ave(dayt$gddbase_cab, by = list(dayt$year, dayt$phen_stage_cab), FUN = cumsum)

#Summation by year and variety
#merlot
hat$gdd_year_mer <- ave(hat$gddbase_mer, hat$year, FUN = cumsum)
dayt$gdd_year_mer <- ave(dayt$gddbase_mer, dayt$year, FUN = cumsum)
#syrah
hat$gdd_year_syrah <- ave(hat$gddbase_syrah, hat$year, FUN = cumsum)
dayt$gdd_year_syrah <- ave(dayt$gddbase_syrah, dayt$year, FUN = cumsum)
#cabernet
hat$gdd_year_cab <- ave(hat$gddbase_cab, hat$year, FUN = cumsum)
dayt$gdd_year_cab <- ave(dayt$gddbase_cab, dayt$year, FUN = cumsum)

#creating dataframe of agg gdd from each variety by year and phen stage at each location

#Phen Stage

#cabernet
ag_hat_phen_cab <- aggregate(hat$gddbase_cab, by = list(hat$year, hat$phen_stage_cab), FUN=sum) 
colnames(ag_hat_phen_cab)[1] <- c("Vintage")
colnames(ag_hat_phen_cab)[2] <- c("phen_stage")
colnames(ag_hat_phen_cab)[3] <- c("hat_gdd_cab")

ag_dayt_phen_cab <- aggregate(dayt$gddbase_cab, by = list(dayt$year, dayt$phen_stage_cab), FUN=sum) 
colnames(ag_dayt_phen_cab)[1] <- c("Vintage")
colnames(ag_dayt_phen_cab)[2] <- c("phen_stage")
colnames(ag_dayt_phen_cab)[3] <- c("dayt_gdd_cab")

#syrah
ag_hat_phen_syrah <- aggregate(hat$gddbase_syrah, by = list(hat$year, hat$phen_stage_syrah), FUN=sum) 
colnames(ag_hat_phen_syrah)[1] <- c("Vintage")
colnames(ag_hat_phen_syrah)[2] <- c("phen_stage")
colnames(ag_hat_phen_syrah)[3] <- c("hat_gdd_syrah")

ag_dayt_phen_syrah <- aggregate(dayt$gddbase_syrah, by = list(dayt$year, dayt$phen_stage_syrah), FUN=sum) 
colnames(ag_dayt_phen_syrah)[1] <- c("Vintage")
colnames(ag_dayt_phen_syrah)[2] <- c("phen_stage")
colnames(ag_dayt_phen_syrah)[3] <- c("dayt_gdd_syrah")

#merlot
ag_hat_phen_mer <- aggregate(hat$gddbase_mer, by = list(hat$year, hat$phen_stage_mer), FUN=sum) 
colnames(ag_hat_phen_mer)[1] <- c("Vintage")
colnames(ag_hat_phen_mer)[2] <- c("phen_stage")
colnames(ag_hat_phen_mer)[3] <- c("hat_gdd_mer")

ag_dayt_phen_mer <- aggregate(dayt$gddbase_mer, by = list(dayt$year, dayt$phen_stage_mer), FUN=sum) 
colnames(ag_dayt_phen_mer)[1] <- c("Vintage")
colnames(ag_dayt_phen_mer)[2] <- c("phen_stage")
colnames(ag_dayt_phen_mer)[3] <- c("dayt_gdd_mer")

gdd_agg_phen <- merge(ag_hat_phen_cab, ag_dayt_phen_cab)
gdd_agg_phen <- merge(gdd_agg_phen, ag_dayt_phen_syrah)
gdd_agg_phen <- merge(gdd_agg_phen, ag_hat_phen_syrah)
gdd_agg_phen <- merge(gdd_agg_phen, ag_dayt_phen_mer)
gdd_agg_phen <- merge(gdd_agg_phen, ag_hat_phen_mer)

#averaging by location for each variety
gdd_agg_phen$gdd_avg_phen_cab <- rowMeans(gdd_agg_phen[, 3:4])
gdd_agg_phen$gdd_avg_phen_syrah <- rowMeans(gdd_agg_phen[, 5:6])
gdd_agg_phen$gdd_avg_phen_mer <- rowMeans(gdd_agg_phen[, 7:8])

#Year

#cabernet
ag_hat_year_cab <- aggregate(hat$gddbase_cab, by = list(hat$year), FUN=sum) 
colnames(ag_hat_year_cab)[1] <- c("Vintage")
colnames(ag_hat_year_cab)[2] <- c("hat_gdd_cab")

ag_dayt_year_cab <- aggregate(dayt$gddbase_cab, by = list(dayt$year), FUN=sum) 
colnames(ag_dayt_year_cab)[1] <- c("Vintage")
colnames(ag_dayt_year_cab)[2] <- c("dayt_gdd_cab")

#syrah
ag_hat_year_syrah <- aggregate(hat$gddbase_syrah, by = list(hat$year), FUN=sum) 
colnames(ag_hat_year_syrah)[1] <- c("Vintage")
colnames(ag_hat_year_syrah)[2] <- c("hat_gdd_syrah")

ag_dayt_year_syrah <- aggregate(dayt$gddbase_syrah, by = list(dayt$year), FUN=sum) 
colnames(ag_dayt_year_syrah)[1] <- c("Vintage")
colnames(ag_dayt_year_syrah)[2] <- c("dayt_gdd_syrah")

#merlot
ag_hat_year_mer <- aggregate(hat$gddbase_mer, by = list(hat$year), FUN=sum) 
colnames(ag_hat_year_mer)[1] <- c("Vintage")
colnames(ag_hat_year_mer)[2] <- c("hat_gdd_mer")

ag_dayt_year_mer <- aggregate(dayt$gddbase_mer, by = list(dayt$year), FUN=sum) 
colnames(ag_dayt_year_mer)[1] <- c("Vintage")
colnames(ag_dayt_year_mer)[2] <- c("dayt_gdd_mer")

gdd_agg_year <- merge(ag_hat_year_cab, ag_dayt_year_cab)
gdd_agg_year <- merge(gdd_agg_year, ag_hat_year_syrah)
gdd_agg_year <- merge(gdd_agg_year, ag_dayt_year_syrah)
gdd_agg_year <- merge(gdd_agg_year, ag_hat_year_mer)
gdd_agg_year <- merge(gdd_agg_year, ag_dayt_year_mer)

#averaging by location for each variety
gdd_agg_year$gdd_avg_year_cab <- rowMeans(gdd_agg_year[, 2:3])
gdd_agg_year$gdd_avg_year_syrah <- rowMeans(gdd_agg_year[, 4:5])
gdd_agg_year$gdd_avg_year_mer <- rowMeans(gdd_agg_year[, 6:7])

#Calculating Precipitation at each location by year and phen stage

#Base GDD values - need individual per variety
#syrah
hat$prcpbase_syrah <- ifelse(is.na(hat$phen_stage_syrah), 0, hat$PRCP)
dayt$prcpbase_syrah <- ifelse(is.na(dayt$phen_stage_syrah), 0, dayt$PRCP)
#cabernet
hat$prcpbase_cab <- ifelse(is.na(hat$phen_stage_cab), 0, hat$PRCP)
dayt$prcpbase_cab <- ifelse(is.na(dayt$phen_stage_cab), 0, dayt$PRCP)
#merlot
hat$prcpbase_mer <- ifelse(is.na(hat$phen_stage_mer), 0, hat$PRCP)
dayt$prcpbase_mer <- ifelse(is.na(dayt$phen_stage_mer), 0, dayt$PRCP)

#Summation by phenological stage and variety
#merlot
hat$prcpsum_phen_mer <- ave(hat$prcpbase_mer, by = list(hat$year, hat$phen_stage_mer), FUN = cumsum)
dayt$prcpsum_phen_mer <- ave(dayt$prcpbase_mer, by = list(dayt$year, dayt$phen_stage_mer), FUN = cumsum)
#syrah
hat$prcpsum_phen_syrah <- ave(hat$prcpbase_syrah, by = list(hat$year, hat$phen_stage_syrah), FUN = cumsum)
dayt$prcpsum_phen_syrah <- ave(dayt$prcpbase_syrah, by = list(dayt$year, dayt$phen_stage_syrah), FUN = cumsum)
#cabernet
hat$prcpsum_phen_cab <- ave(hat$prcpbase_cab, by = list(hat$year, hat$phen_stage_cab), FUN = cumsum)
dayt$prcpsum_phen_cab <- ave(dayt$prcpbase_cab, by = list(dayt$year, dayt$phen_stage_cab), FUN = cumsum)

#Summation by year and variety
#merlot
hat$prcpsum_year_mer <- ave(hat$prcpbase_mer, hat$year, FUN = cumsum)
dayt$prcpsum_year_mer <- ave(dayt$prcpbase_mer, dayt$year, FUN = cumsum)
#syrah
hat$prcpsum_year_syrah <- ave(hat$prcpbase_syrah, hat$year, FUN = cumsum)
dayt$prcpsum_year_syrah <- ave(dayt$prcpbase_syrah, dayt$year, FUN = cumsum)
#cabernet
hat$prcpsum_year_cab <- ave(hat$prcpbase_cab, hat$year, FUN = cumsum)
dayt$prcpsum_year_cab <- ave(dayt$prcpbase_cab, dayt$year, FUN = cumsum)

#Creating dataframe of agg precipitation from each year and phen stage and each location

#By Phen Stage
#cabernet
agp_hat_phen_cab <- aggregate(hat$prcpbase_cab, by = list(hat$year, hat$phen_stage_cab), FUN=sum) 
colnames(agp_hat_phen_cab)[1] <- c("Vintage")
colnames(agp_hat_phen_cab)[2] <- c("phen_stage")
colnames(agp_hat_phen_cab)[3] <- c("hat_prcp_cab")

agp_dayt_phen_cab <- aggregate(dayt$prcpbase_cab, by = list(dayt$year, dayt$phen_stage_cab), FUN=sum) 
colnames(agp_dayt_phen_cab)[1] <- c("Vintage")
colnames(agp_dayt_phen_cab)[2] <- c("phen_stage")
colnames(agp_dayt_phen_cab)[3] <- c("dayt_prcp_cab")

#syrah
agp_hat_phen_syrah <- aggregate(hat$prcpbase_syrah, by = list(hat$year, hat$phen_stage_syrah), FUN=sum) 
colnames(agp_hat_phen_syrah)[1] <- c("Vintage")
colnames(agp_hat_phen_syrah)[2] <- c("phen_stage")
colnames(agp_hat_phen_syrah)[3] <- c("hat_prcp_syrah")

agp_dayt_phen_syrah <- aggregate(dayt$prcpbase_syrah, by = list(dayt$year, dayt$phen_stage_syrah), FUN=sum) 
colnames(agp_dayt_phen_syrah)[1] <- c("Vintage")
colnames(agp_dayt_phen_syrah)[2] <- c("phen_stage")
colnames(agp_dayt_phen_syrah)[3] <- c("dayt_prcp_syrah")

#merlot
agp_hat_phen_mer <- aggregate(hat$prcpbase_mer, by = list(hat$year, hat$phen_stage_mer), FUN=sum) 
colnames(agp_hat_phen_mer)[1] <- c("Vintage")
colnames(agp_hat_phen_mer)[2] <- c("phen_stage")
colnames(agp_hat_phen_mer)[3] <- c("hat_prcp_mer")

agp_dayt_phen_mer <- aggregate(dayt$prcpbase_mer, by = list(dayt$year, dayt$phen_stage_mer), FUN=sum) 
colnames(agp_dayt_phen_mer)[1] <- c("Vintage")
colnames(agp_dayt_phen_mer)[2] <- c("phen_stage")
colnames(agp_dayt_phen_mer)[3] <- c("dayt_prcp_mer")

prcp_agg_phen <- merge(agp_hat_phen_cab, agp_dayt_phen_cab)
prcp_agg_phen <- merge(prcp_agg_phen, agp_dayt_phen_syrah)
prcp_agg_phen <- merge(prcp_agg_phen, agp_hat_phen_syrah)
prcp_agg_phen <- merge(prcp_agg_phen, agp_dayt_phen_mer)
prcp_agg_phen <- merge(prcp_agg_phen, agp_hat_phen_mer)

#averaging by location for each variety
prcp_agg_phen$prcp_avg_phen_cab <- rowMeans(prcp_agg_phen[, 3:4])
prcp_agg_phen$prcp_avg_phen_syrah <- rowMeans(prcp_agg_phen[, 5:6])
prcp_agg_phen$prcp_avg_phen_mer <- rowMeans(prcp_agg_phen[, 7:8])

#By Year

#cabernet
agp_hat_year_cab <- aggregate(hat$prcpbase_cab, by = list(hat$year), FUN=sum) 
colnames(agp_hat_year_cab)[1] <- c("Vintage")
colnames(agp_hat_year_cab)[2] <- c("hat_prcp_cab")

agp_dayt_year_cab <- aggregate(dayt$prcpbase_cab, by = list(dayt$year), FUN=sum) 
colnames(agp_dayt_year_cab)[1] <- c("Vintage")
colnames(agp_dayt_year_cab)[2] <- c("dayt_prcp_cab")

#syrah
agp_hat_year_syrah <- aggregate(hat$prcpbase_syrah, by = list(hat$year), FUN=sum) 
colnames(agp_hat_year_syrah)[1] <- c("Vintage")
colnames(agp_hat_year_syrah)[2] <- c("hat_prcp_syrah")

agp_dayt_year_syrah <- aggregate(dayt$prcpbase_syrah, by = list(dayt$year), FUN=sum) 
colnames(agp_dayt_year_syrah)[1] <- c("Vintage")
colnames(agp_dayt_year_syrah)[2] <- c("dayt_prcp_syrah")

#merlot
agp_hat_year_mer <- aggregate(hat$prcpbase_mer, by = list(hat$year), FUN=sum) 
colnames(agp_hat_year_mer)[1] <- c("Vintage")
colnames(agp_hat_year_mer)[2] <- c("hat_prcp_mer")

agp_dayt_year_mer <- aggregate(dayt$prcpbase_mer, by = list(dayt$year), FUN=sum) 
colnames(agp_dayt_year_mer)[1] <- c("Vintage")
colnames(agp_dayt_year_mer)[2] <- c("dayt_prcp_mer")

prcp_agg_year <- merge(agp_hat_year_cab, agp_dayt_year_cab)
prcp_agg_year <- merge(prcp_agg_year, agp_hat_year_syrah)
prcp_agg_year <- merge(prcp_agg_year, agp_dayt_year_syrah)
prcp_agg_year <- merge(prcp_agg_year, agp_hat_year_mer)
prcp_agg_year <- merge(prcp_agg_year, agp_dayt_year_mer)

#averaging by location for each variety
prcp_agg_year$prcp_avg_year_cab <- rowMeans(prcp_agg_year[, 2:3])
prcp_agg_year$prcp_avg_year_syrah <- rowMeans(prcp_agg_year[, 4:5])
prcp_agg_year$prcp_avg_year_mer <- rowMeans(prcp_agg_year[, 6:7])

#Subsetting by Variety and location
unique(mydat$Variety)

#variety
Merlot <- subset(mydat, mydat$Variety=="Merlot")
Cabernet <- subset(mydat, mydat$Variety=="Cabernet")
Syrah <- subset(mydat, mydat$Variety=="Syrah")

#creating 2 dataframes. One by year, one by phen. Both with gdd_agg, prcp_agg, vintage, and napa ratings
Agg_Table_year <- merge(prcp_agg_year, gdd_agg_year)
Agg_Table_phen <- merge(prcp_agg_phen, gdd_agg_phen)

#year
Syrah_Table <- merge(Agg_Table_year, Syrah) #values same as gen
Syrah_Table <- Syrah_Table[,-c(2,3,4,5,6,7,8,10,11,12,13,14,15,16,17,19)] #keeping averages
colnames(Syrah_Table)[3] <- c("var_gdd_avg_year")
colnames(Syrah_Table)[2] <- c("var_prcp_avg_year")

Merlot_Table <- merge(Agg_Table_year, Merlot)
Merlot_Table <- Merlot_Table[,-c(2,3,4,5,6,7,8,9,11,12,13,14,15,16,17,18)] #keeping averages
colnames(Merlot_Table)[2] <- c("var_prcp_avg_year")
colnames(Merlot_Table)[3] <- c("var_gdd_avg_year")

Cabernet_Table <- merge(Agg_Table_year, Cabernet) #values from gen
Cabernet_Table <- Cabernet_Table[,-c(2,3,4,5,6,7,9,10,11,12,13,14,15,16,18,19)] #keeping averages
colnames(Cabernet_Table)[2] <- c("var_prcp_avg_year")
colnames(Cabernet_Table)[3] <- c("var_gdd_avg_year")

WA_Table_Full_Year <- rbind(Syrah_Table, Merlot_Table, Cabernet_Table)
WA_Table_Full_Year <- WA_Table_Full_Year[,c(1,4,5,6,7,8,9,2,3,10,11)] #reorganize columns

#phen
Syrah_Table <- merge(Agg_Table_phen, Syrah) #values from gen
Syrah_Table <- Syrah_Table[,-c(3,4,5,6,7,8,9,11,12,13,14,15,16,17,18,20)] #keeping averages
colnames(Syrah_Table)[4] <- c("var_gdd_avg_phen")
colnames(Syrah_Table)[3] <- c("var_prcp_avg_phen")

Merlot_Table <- merge(Agg_Table_phen, Merlot)
Merlot_Table <- Merlot_Table[,-c(3,4,5,6,7,8,9,10,12,13,14,15,16,17,18,19)] #keeping averages
colnames(Merlot_Table)[4] <- c("var_gdd_avg_phen")
colnames(Merlot_Table)[3] <- c("var_prcp_avg_phen")

Cabernet_Table <- merge(Agg_Table_phen, Cabernet) #values from gen
Cabernet_Table <- Cabernet_Table[,-c(3,4,5,6,7,8,10,11,12,13,14,15,16,17,19,20)] #keeping averages
colnames(Cabernet_Table)[4] <- c("var_gdd_avg_phen")
colnames(Cabernet_Table)[3] <- c("var_prcp_avg_phen")

WA_Table_Full_Phen <- rbind(Syrah_Table, Merlot_Table, Cabernet_Table)
WA_Table_Full_Phen <- pivot_wider(WA_Table_Full_Phen, names_from = phen_stage, values_from = c(var_prcp_avg_phen, var_gdd_avg_phen))
WA_Table_Full_Phen <- WA_Table_Full_Phen[,c(1,2,3,4,5,6,7,10,11,12,13,14,15,8,9)] #reorganize columns

#Exporting as csv for future modeling
write.csv(WA_Table_Full_Year,"/Users/phoebeautio/Desktop/Vintages/data/NapaClimate2021/TablesForModels/WAComplete_year.csv", row.names = FALSE)
write.csv(WA_Table_Full_Phen,"/Users/phoebeautio/Desktop/Vintages/data/NapaClimate2021/TablesForModels/WAComplete_phen.csv", row.names = FALSE)
