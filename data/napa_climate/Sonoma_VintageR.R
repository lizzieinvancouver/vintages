################# Sonoma Vintage Dataset (PA) - Updated 3/14/2021 #########################

#Housekeeping
rm(list=ls())
options(stringsAsFactors = FALSE)

#Load libraries
library(lubridate)
library(dplyr)

#Reading in csv files
mydat <- read.csv("/Users/phoebeautio/Desktop/Vintage Research/Napa_Vintage.csv", header=TRUE, na.strings=c(""," ","NA"))
head(mydat)

sono <- read.csv("/Users/phoebeautio/Desktop/Vintage Research/SonomaClimateClean.csv", header=TRUE, na.strings=c(""," ","NA"))
head(sono)

#Adding Phenology Column (by Variety)
sono['phen_stage'] <- NA
sono['phen_stage_cab'] <- NA
sono['phen_stage_chard'] <- NA
sono['phen_stage_mer'] <- NA
#sono['phen_stage_zin'] <- NA
#sono['phen_stage_rhone'] <- NA

#General
#Budburst (1)
sono$phen_stage[which(sono$month=="4")] <- "1"
sono$phen_stage[which(sono$month=="5" & sono$day %in% c("1","2","3","4","5","6","7","8","9","10","11","12","13","14","15"))] <- "1"

#Bloom (2)
sono$phen_stage[which(sono$month=="5" & sono$day %in% c("16","17","18","19","20","21","22",
                                                                 "23","24","25","26","27","28","29","30","31"))] <- "2"
sono$phen_stage[which(sono$month=="6")] <- "2"
sono$phen_stage[which(sono$month=="7" & sono$day %in% c("1","2","3","4","5","6","7","8","9",
                                                                 "10","11","12","13","14","15","16",
                                                                 "17","18","19","20","21","22","23"))] <- "2"
#Veraison (3)
sono$phen_stage[which(sono$month=="7" & sono$day %in% c("24","25","26","27","28","29","30","31"))] <- "3"
sono$phen_stage[which(sono$month=="8")] <- "3"
sono$phen_stage[which(sono$month=="9" & sono$day %in% c("1","2","3","4","5","6","7","8","9",
                                                                 "10","11","12","13","14","15"))] <- "3"
#Chardonnay
#Budburst (1)
sono$phen_stage_chard[which(sono$month=="3" & sono$day %in% c("16","17","18","19","20","21","22",
                                                                       "23","24","25","26","27","28","29","30","31"))] <- "1"
sono$phen_stage_chard[which(sono$month=="4")] <- "1"
sono$phen_stage_chard[which(sono$month=="5" & sono$day %in% c("1","2","3","4","5"))] <- "1"

#Bloom (2)
sono$phen_stage_chard[which(sono$month=="5" & sono$day %in% c("6","7","8","9","10","11","12","13","14","15","16","17","18","19","20","21","22",
                                                                       "23","24","25","26","27","28","29","30","31"))] <- "2"
sono$phen_stage_chard[which(sono$month=="6")] <- "2"
sono$phen_stage_chard[which(sono$month=="7" & sono$day %in% c("1","2","3","4","5","6","7","8","9","10"))] <- "2"

#Veraison (3)
sono$phen_stage_chard[which(sono$month=="7" & sono$day %in% c("11","12","13","14","15","16","17","18","19","20","21","22","23",
                                                                       "24","25","26","27","28","29","30","31"))] <- "3"
sono$phen_stage_chard[which(sono$month=="8")] <- "3"
sono$phen_stage_chard[which(sono$month=="9" & sono$day %in% c("1","2","3","4","5","6","7","8","9",
                                                                       "10","11","12","13","14","15"))] <- "3"

#Cabernet
#Budburst (1)
sono$phen_stage_cab[which(sono$month=="3" & sono$day %in% c("25","26","27","28","29","30","31"))] <- "1"
sono$phen_stage_cab[which(sono$month=="4")] <- "1"
sono$phen_stage_cab[which(sono$month=="5" & sono$day %in% c("1","2","3","4","5","6","7","8","9","10","11","12","13","14","15"))] <- "1"

#Bloom (2)
sono$phen_stage_cab[which(sono$month=="5" & sono$day %in% c("16","17","18","19","20","21","22",
                                                                     "23","24","25","26","27","28","29","30","31"))] <- "2"
sono$phen_stage_cab[which(sono$month=="6")] <- "2"
sono$phen_stage_cab[which(sono$month=="7" & sono$day %in% c("1","2","3","4","5","6","7","8","9",
                                                                     "10","11","12","13","14","15","16",
                                                                     "17","18","19","20","21","22","23"))] <- "2"

#Veraison (3)
sono$phen_stage_cab[which(sono$month=="7" & sono$day %in% c("23","24","25","26","27","28","29","30","31"))] <- "3"
sono$phen_stage_cab[which(sono$month=="8")] <- "3"
sono$phen_stage_cab[which(sono$month=="9" & sono$day %in% c("1","2","3","4","5","6","7","8","9",
                                                                     "10","11","12","13","14","15","16",
                                                                     "17","18","19","20","21","22","23"))] <- "3"

#Merlot
#Budburst (1)
sono$phen_stage_mer[which(sono$month=="3" & sono$day %in% c("25","26","27","28","29","30","31"))] <- "1"
sono$phen_stage_mer[which(sono$month=="4")] <- "1"
sono$phen_stage_mer[which(sono$month=="5" & sono$day %in% c("1","2","3","4","5","6","7","8","9","10","11","12","13"))] <- "1"

#Bloom (2)
sono$phen_stage_mer[which(sono$month=="5" & sono$day %in% c("14","15","16","17","18","19","20","21","22",
                                                                     "23","24","25","26","27","28","29","30","31"))] <- "2"
sono$phen_stage_mer[which(sono$month=="6")] <- "2"
sono$phen_stage_mer[which(sono$month=="7" & sono$day %in% c("1","2","3","4","5","6","7","8","9",
                                                                     "10","11","12","13","14","15"))] <- "2"

#Veraison (3)
sono$phen_stage_mer[which(sono$month=="7" & sono$day %in% c("16","17","18","19","20","21","22","23","24","25",
                                                                     "26","27","28","29","30","31"))] <- "3"
sono$phen_stage_mer[which(sono$month=="8")] <- "3"
sono$phen_stage_mer[which(sono$month=="9" & sono$day %in% c("1","2","3","4","5","6","7","8","9",
                                                                     "10","11","12","13","14","15"))] <- "3"

#remove empty rows
sono <- sono[!(is.na(sono$phen_stage) & is.na(sono$phen_stage_cab) & is.na(sono$phen_stage_chard) & is.na(sono$phen_stage_mer)), ]

#Calculating GDD at each location -- GDD base temp = 10
#Base GDD values - need individual per variety
#general
sono$gddbase <- ifelse(!is.na(sono$phen_stage) & sono$TAVG >= 10, sono$TAVG - 10, 0)
#chardonnay
sono$gddbase_chard <- ifelse(!is.na(sono$phen_stage_chard) & sono$TAVG >= 10, sono$TAVG - 10, 0)
#cabernet
sono$gddbase_cab <- ifelse(!is.na(sono$phen_stage_cab) & sono$TAVG >= 10, sono$TAVG - 10, 0)
#merlot
sono$gddbase_mer <- ifelse(!is.na(sono$phen_stage_mer) & sono$TAVG >= 10, sono$TAVG - 10, 0)

#Summation by phenological stage and variety
#general
sono$gdd_phen <- ave(sono$gddbase, by = list(sono$year, sono$phen_stage), FUN = cumsum)
#merlot
sono$gdd_phen_mer <- ave(sono$gddbase_mer, by = list(sono$year, sono$phen_stage_mer), FUN = cumsum)
#chardonnay
sono$gdd_phen_chard <- ave(sono$gddbase_chard, by = list(sono$year, sono$phen_stage_chard), FUN = cumsum)
#cabernet
sono$gdd_phen_cab <- ave(sono$gddbase_cab, by = list(sono$year, sono$phen_stage_cab), FUN = cumsum)

#Summation by year and variety
#general
sono$gdd_year <- ave(sono$gddbase, sono$year, FUN = cumsum)
#merlot
sono$gdd_year_mer <- ave(sono$gddbase_mer, sono$year, FUN = cumsum)
#chardonnay
sono$gdd_year_chard <- ave(sono$gddbase_chard, sono$year, FUN = cumsum)
#cabernet
sono$gdd_year_cab <- ave(sono$gddbase_cab, sono$year, FUN = cumsum)

#creating dataframe of agg gdd from each variety by year and phen stage at each location

#Phen Stage
#general
ag_sono_phen_gen <- aggregate(sono$gddbase, by = list(sono$year, sono$phen_stage), FUN=sum) 
colnames(ag_sono_phen_gen)[1] <- c("Vintage")
colnames(ag_sono_phen_gen)[2] <- c("phen_stage")
colnames(ag_sono_phen_gen)[3] <- c("sono_gdd")

#cabernet
ag_sono_phen_cab <- aggregate(sono$gddbase_cab, by = list(sono$year, sono$phen_stage_cab), FUN=sum) 
colnames(ag_sono_phen_cab)[1] <- c("Vintage")
colnames(ag_sono_phen_cab)[2] <- c("phen_stage")
colnames(ag_sono_phen_cab)[3] <- c("sono_gdd_cab")

#chardonnay
ag_sono_phen_chard <- aggregate(sono$gddbase_chard, by = list(sono$year, sono$phen_stage_chard), FUN=sum) 
colnames(ag_sono_phen_chard)[1] <- c("Vintage")
colnames(ag_sono_phen_chard)[2] <- c("phen_stage")
colnames(ag_sono_phen_chard)[3] <- c("sono_gdd_chard")

#merlot
ag_sono_phen_mer <- aggregate(sono$gddbase_mer, by = list(sono$year, sono$phen_stage_mer), FUN=sum) 
colnames(ag_sono_phen_mer)[1] <- c("Vintage")
colnames(ag_sono_phen_mer)[2] <- c("phen_stage")
colnames(ag_sono_phen_mer)[3] <- c("sono_gdd_mer")

gdd_agg_phen <- merge(ag_sono_phen_gen, ag_sono_phen_cab)
gdd_agg_phen <- merge(gdd_agg_phen, ag_sono_phen_chard)
gdd_agg_phen <- merge(gdd_agg_phen, ag_sono_phen_mer)

#Year
#gen
ag_sono_year_gen <- aggregate(sono$gddbase, by = list(sono$year), FUN=sum) 
colnames(ag_sono_year_gen)[1] <- c("Vintage")
colnames(ag_sono_year_gen)[2] <- c("sono_gdd")

#cabernet
ag_sono_year_cab <- aggregate(sono$gddbase_cab, by = list(sono$year), FUN=sum) 
colnames(ag_sono_year_cab)[1] <- c("Vintage")
colnames(ag_sono_year_cab)[2] <- c("sono_gdd_cab")

#chardonnay
ag_sono_year_chard <- aggregate(sono$gddbase_chard, by = list(sono$year), FUN=sum) 
colnames(ag_sono_year_chard)[1] <- c("Vintage")
colnames(ag_sono_year_chard)[2] <- c("sono_gdd_chard")

#merlot
ag_sono_year_mer <- aggregate(sono$gddbase_mer, by = list(sono$year), FUN=sum) 
colnames(ag_sono_year_mer)[1] <- c("Vintage")
colnames(ag_sono_year_mer)[2] <- c("sono_gdd_mer")

gdd_agg_year <- merge(ag_sono_year_gen, ag_sono_year_cab)
gdd_agg_year <- merge(gdd_agg_year, ag_sono_year_chard)
gdd_agg_year <- merge(gdd_agg_year, ag_sono_year_mer)

#Calculating Precipitation at each location by year and phen stage

#Base GDD values - need individual per variety
#general
sono$prcpbase_gen <- ifelse(is.na(sono$phen_stage), 0, sono$PRCP)
#chardonnay
sono$prcpbase_chard <- ifelse(is.na(sono$phen_stage_chard), 0, sono$PRCP)
#cabernet
sono$prcpbase_cab <- ifelse(is.na(sono$phen_stage_cab), 0, sono$PRCP)
#merlot
sono$prcpbase_mer <- ifelse(is.na(sono$phen_stage_mer), 0, sono$PRCP)

#Summation by phenological stage and variety
#general
sono$prcpsum_phen <- ave(sono$prcpbase_gen, by = list(sono$year, sono$phen_stage), FUN = cumsum)
#merlot
sono$prcpsum_phen_mer <- ave(sono$prcpbase_mer, by = list(sono$year, sono$phen_stage_mer), FUN = cumsum)
#chardonnay
sono$prcpsum_phen_chard <- ave(sono$prcpbase_chard, by = list(sono$year, sono$phen_stage_chard), FUN = cumsum)
#cabernet
sono$prcpsum_phen_cab <- ave(sono$prcpbase_cab, by = list(sono$year, sono$phen_stage_cab), FUN = cumsum)

#Summation by year and variety
#general
sono$prcpsum_year <- ave(sono$prcpbase_gen, sono$year, FUN = cumsum)
#merlot
sono$prcpsum_year_mer <- ave(sono$prcpbase_mer, sono$year, FUN = cumsum)
#chardonnay
sono$prcpsum_year_chard <- ave(sono$prcpbase_chard, sono$year, FUN = cumsum)
#cabernet
sono$prcpsum_year_cab <- ave(sono$prcpbase_cab, sono$year, FUN = cumsum)

#Creating dataframe of agg precipitation from each year and phen stage and each location

#By Phen Stage
#general
agp_sono_phen_gen <- aggregate(sono$prcpbase_gen, by = list(sono$year, sono$phen_stage), FUN=sum) 
colnames(agp_sono_phen_gen)[1] <- c("Vintage")
colnames(agp_sono_phen_gen)[2] <- c("phen_stage")
colnames(agp_sono_phen_gen)[3] <- c("sono_prcp")

#cabernet
agp_sono_phen_cab <- aggregate(sono$prcpbase_cab, by = list(sono$year, sono$phen_stage_cab), FUN=sum) 
colnames(agp_sono_phen_cab)[1] <- c("Vintage")
colnames(agp_sono_phen_cab)[2] <- c("phen_stage")
colnames(agp_sono_phen_cab)[3] <- c("sono_prcp_cab")

#chardonnay
agp_sono_phen_chard <- aggregate(sono$prcpbase_chard, by = list(sono$year, sono$phen_stage_chard), FUN=sum) 
colnames(agp_sono_phen_chard)[1] <- c("Vintage")
colnames(agp_sono_phen_chard)[2] <- c("phen_stage")
colnames(agp_sono_phen_chard)[3] <- c("sono_prcp_chard")

#merlot
agp_sono_phen_mer <- aggregate(sono$prcpbase_mer, by = list(sono$year, sono$phen_stage_mer), FUN=sum) 
colnames(agp_sono_phen_mer)[1] <- c("Vintage")
colnames(agp_sono_phen_mer)[2] <- c("phen_stage")
colnames(agp_sono_phen_mer)[3] <- c("sono_prcp_mer")

prcp_agg_phen <- merge(agp_sono_phen_gen, agp_sono_phen_cab)
prcp_agg_phen <- merge(prcp_agg_phen, agp_sono_phen_chard)
prcp_agg_phen <- merge(prcp_agg_phen, agp_sono_phen_mer)

#By Year
#gen
agp_sono_year_gen <- aggregate(sono$prcpbase_gen, by = list(sono$year), FUN=sum) 
colnames(agp_sono_year_gen)[1] <- c("Vintage")
colnames(agp_sono_year_gen)[2] <- c("sono_prcp")

#cabernet
agp_sono_year_cab <- aggregate(sono$prcpbase_cab, by = list(sono$year), FUN=sum) 
colnames(agp_sono_year_cab)[1] <- c("Vintage")
colnames(agp_sono_year_cab)[2] <- c("sono_prcp_cab")

#chardonnay
agp_sono_year_chard <- aggregate(sono$prcpbase_chard, by = list(sono$year), FUN=sum) 
colnames(agp_sono_year_chard)[1] <- c("Vintage")
colnames(agp_sono_year_chard)[2] <- c("sono_prcp_chard")

#merlot
agp_sono_year_mer <- aggregate(sono$prcpbase_mer, by = list(sono$year), FUN=sum) 
colnames(agp_sono_year_mer)[1] <- c("Vintage")
colnames(agp_sono_year_mer)[2] <- c("sono_prcp_mer")

prcp_agg_year <- merge(agp_sono_year_gen, agp_sono_year_cab)
prcp_agg_year <- merge(prcp_agg_year, agp_sono_year_chard)
prcp_agg_year <- merge(prcp_agg_year, agp_sono_year_mer)

#Subsetting by location (Napa/Sonoma/North Coast)
#Editing Rhône character
mydat$Variety[which(mydat$Variety=="Rhône-Style Reds")] <- "Rhone"

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

#location - sonoma
sonoma_cabernet <- subset(sonoma, sonoma$Variety=="Cabernet")
sonoma_chardonnay <- subset(sonoma, sonoma$Variety=="Chardonnay")
sonoma_rhone <- subset(sonoma, sonoma$Variety=="Rhone")
sonoma_merlot <- subset(sonoma, sonoma$Variety=="Merlot")
sonoma_zinfandel <- subset(sonoma, sonoma$Variety=="Zinfandel")

#creating 2 dataframes. One by year, one by phen. Both with gdd_agg, prcp_agg, vintage, and napa ratings
Agg_Table_year <- merge(prcp_agg_year, gdd_agg_year)
Agg_Table_phen <- merge(prcp_agg_phen, gdd_agg_phen)

#sonoma - year
Cabernet_Table_S <- merge(Agg_Table_year, sonoma_cabernet)
Cabernet_Table_S <- Cabernet_Table_S[,-c(2,4,5,6,8,9)] #keeping var
colnames(Cabernet_Table_S)[3] <- c("var_gdd_avg_year")
colnames(Cabernet_Table_S)[2] <- c("var_prcp_avg_year")

Chardonnay_Table_S <- merge(Agg_Table_year, sonoma_chardonnay)
Chardonnay_Table_S <- Chardonnay_Table_S[,-c(2,3,5,6,7,9)] #keeping var
colnames(Chardonnay_Table_S)[2] <- c("var_prcp_avg_year")
colnames(Chardonnay_Table_S)[3] <- c("var_gdd_avg_year")

Merlot_Table_S <- merge(Agg_Table_year, sonoma_merlot)
Merlot_Table_S <- Merlot_Table_S[,-c(2,3,4,6,7,8)] #keeping var
colnames(Merlot_Table_S)[2] <- c("var_prcp_avg_year")
colnames(Merlot_Table_S)[3] <- c("var_gdd_avg_year")

Rhone_Table_S <- merge(Agg_Table_year, sonoma_rhone) #values from gen
Rhone_Table_S <- Rhone_Table_S[,-c(3,4,5,7,8,9)] #keeping var
colnames(Rhone_Table_S)[2] <- c("var_prcp_avg_year")
colnames(Rhone_Table_S)[3] <- c("var_gdd_avg_year")

Zinfandel_Table_S <- merge(Agg_Table_year, sonoma_zinfandel) #values from gen
Zinfandel_Table_S <- Zinfandel_Table_S[,-c(3,4,5,7,8,9)] #keeping var
colnames(Zinfandel_Table_S)[2] <- c("var_prcp_avg_year")
colnames(Zinfandel_Table_S)[3] <- c("var_gdd_avg_year")

Sonoma_Table_Full_Year <- rbind(Cabernet_Table_S, Chardonnay_Table_S, Merlot_Table_S, Rhone_Table_S, Zinfandel_Table_S)
Sonoma_Table_Full_Year <- Sonoma_Table_Full_Year[,c(1,4,5,6,7,8,9,2,3,10,11)] #reorganize columns

#sonoma - phen
Cabernet_Table_S <- merge(Agg_Table_phen, sonoma_cabernet)
Cabernet_Table_S <- Cabernet_Table_S[,-c(3,5,6,7,9,10)] #keeping var
colnames(Cabernet_Table_S)[4] <- c("var_gdd_avg_phen")
colnames(Cabernet_Table_S)[3] <- c("var_prcp_avg_phen")

Chardonnay_Table_S <- merge(Agg_Table_phen, sonoma_chardonnay)
Chardonnay_Table_S <- Chardonnay_Table_S[,-c(3,4,6,7,8,10)] #keeping var
colnames(Chardonnay_Table_S)[4] <- c("var_gdd_avg_phen")
colnames(Chardonnay_Table_S)[3] <- c("var_prcp_avg_phen")

Merlot_Table_S <- merge(Agg_Table_phen, sonoma_merlot)
Merlot_Table_S <- Merlot_Table_S[,-c(3,4,5,7,8,9)] #keeping var
colnames(Merlot_Table_S)[3] <- c("var_prcp_avg_phen")
colnames(Merlot_Table_S)[4] <- c("var_gdd_avg_phen")

Rhone_Table_S <- merge(Agg_Table_phen, sonoma_rhone)
Rhone_Table_S <- Rhone_Table_S[,-c(4,5,6,8,9,10)] #keeping var
colnames(Rhone_Table_S)[4] <- c("var_gdd_avg_phen")
colnames(Rhone_Table_S)[3] <- c("var_prcp_avg_phen")

Zinfandel_Table_S <- merge(Agg_Table_phen, sonoma_zinfandel)
Zinfandel_Table_S <- Zinfandel_Table_S[,-c(4,5,6,8,9,10)] #keeping var
colnames(Zinfandel_Table_S)[4] <- c("var_gdd_avg_phen")
colnames(Zinfandel_Table_S)[3] <- c("var_prcp_avg_phen")

Sonoma_Table_Full_Phen <- rbind(Cabernet_Table_S, Chardonnay_Table_S, Merlot_Table_S, Rhone_Table_S, Zinfandel_Table_S)
Sonoma_Table_Full_Phen <- pivot_wider(Sonoma_Table_Full_Phen, names_from = phen_stage, values_from = c(var_prcp_avg_phen, var_gdd_avg_phen))
Sonoma_Table_Full_Phen <- Sonoma_Table_Full_Phen[,c(1,2,3,4,5,6,7,10,11,12,13,14,15,8,9)] #reorganize columns

#Exporting as csv for future modeling
write.csv(Sonoma_Table_Full_Year,"/Users/phoebeautio/Desktop/Vintage Research/TablesForModels/UniqueSonomaComplete_year.csv", row.names = FALSE)
write.csv(Sonoma_Table_Full_Phen,"/Users/phoebeautio/Desktop/Vintage Research/TablesForModels/UniqueSonomaComplete_phen.csv", row.names = FALSE)
