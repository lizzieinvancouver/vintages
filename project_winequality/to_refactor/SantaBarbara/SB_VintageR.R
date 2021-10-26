################# Santa Barbara Vintage Dataset (PA) - Updated 3/18/2021 #########################

#Housekeeping
rm(list=ls())
options(stringsAsFactors = FALSE)

#Load libraries
library(lubridate)
library(dplyr)
library(tidyr)

#Reading in csv files
mydat <- read.csv("/Users/phoebeautio/Desktop/Vintages/data/NapaClimate2021/SantaBarbara/SB_Vintage.csv", header=TRUE, na.strings=c(""," ","NA"))
head(mydat)

smaria <- read.csv("/Users/phoebeautio/Desktop/Vintages/data/NapaClimate2021/SantaBarbara/SMariaClean.csv", header=TRUE, na.strings=c(""," ","NA"))
head(smaria)

sb <- read.csv("/Users/phoebeautio/Desktop/Vintages/data/NapaClimate2021/SantaBarbara/SBClean.csv", header=TRUE, na.strings=c(""," ","NA"))
head(sb)

## Relative paths (Geoff)
## mydat <- read.csv("Napa_Vintage.csv", header=TRUE, na.strings=c(""," ","NA"))
## head(mydat)

#Adding Phenology Column (by Variety)
sb['phen_stage'] <- NA
sb['phen_stage_chard'] <- NA
#sb['phen_stage_pn'] <- NA
#sb['phen_stage_rhone'] <- NA

smaria['phen_stage'] <- NA
smaria['phen_stage_chard'] <- NA
#smaria['phen_stage_pn'] <- NA
#smaria['phen_stage_rhone'] <- NA

#General (pn and rhone)
#Budburst (1)
sb$phen_stage[which(sb$month=="4")] <- "1"
sb$phen_stage[which(sb$month=="5" & sb$day %in% c("1","2","3","4","5","6","7","8","9","10","11","12","13","14","15"))] <- "1"

smaria$phen_stage[which(smaria$month=="4")] <- "1"
smaria$phen_stage[which(smaria$month=="5" & smaria$day %in% c("1","2","3","4","5","6","7","8","9","10","11","12","13","14","15"))] <- "1"

#Bloom (2)
sb$phen_stage[which(sb$month=="5" & sb$day %in% c("16","17","18","19","20","21","22",
                                                                 "23","24","25","26","27","28","29","30","31"))] <- "2"
sb$phen_stage[which(sb$month=="6")] <- "2"
sb$phen_stage[which(sb$month=="7" & sb$day %in% c("1","2","3","4","5","6","7","8","9",
                                                                 "10","11","12","13","14","15","16",
                                                                 "17","18","19","20","21","22","23"))] <- "2"
smaria$phen_stage[which(smaria$month=="5" & smaria$day %in% c("16","17","18","19","20","21","22",
                                                                       "23","24","25","26","27","28","29","30","31"))] <- "2"
smaria$phen_stage[which(smaria$month=="6")] <- "2"
smaria$phen_stage[which(smaria$month=="7" & smaria$day %in% c("1","2","3","4","5","6","7","8","9",
                                                                       "10","11","12","13","14","15","16",
                                                                       "17","18","19","20","21","22","23"))] <- "2"
#Veraison (3)
sb$phen_stage[which(sb$month=="7" & sb$day %in% c("24","25","26","27","28","29","30","31"))] <- "3"
sb$phen_stage[which(sb$month=="8")] <- "3"
sb$phen_stage[which(sb$month=="9" & sb$day %in% c("1","2","3","4","5","6","7","8","9",
                                                                 "10","11","12","13","14","15"))] <- "3"

smaria$phen_stage[which(smaria$month=="7" & smaria$day %in% c("24","25","26","27","28","29","30","31"))] <- "3"
smaria$phen_stage[which(smaria$month=="8")] <- "3"
smaria$phen_stage[which(smaria$month=="9" & smaria$day %in% c("1","2","3","4","5","6","7","8","9",
                                                                       "10","11","12","13","14","15"))] <- "3"

#Chardonnay
#Budburst (1)
sb$phen_stage_chard[which(sb$month=="3" & sb$day %in% c("16","17","18","19","20","21","22",
                                                                       "23","24","25","26","27","28","29","30","31"))] <- "1"
sb$phen_stage_chard[which(sb$month=="4")] <- "1"
sb$phen_stage_chard[which(sb$month=="5" & sb$day %in% c("1","2","3","4","5"))] <- "1"

smaria$phen_stage_chard[which(smaria$month=="3" & smaria$day %in% c("16","17","18","19","20","21","22",
                                                                             "23","24","25","26","27","28","29","30","31"))] <- "1"
smaria$phen_stage_chard[which(smaria$month=="4")] <- "1"
smaria$phen_stage_chard[which(smaria$month=="5" & smaria$day %in% c("1","2","3","4","5"))] <- "1"

#Bloom (2)
sb$phen_stage_chard[which(sb$month=="5" & sb$day %in% c("6","7","8","9","10","11","12","13","14","15","16","17","18","19","20","21","22",
                                                                       "23","24","25","26","27","28","29","30","31"))] <- "2"
sb$phen_stage_chard[which(sb$month=="6")] <- "2"
sb$phen_stage_chard[which(sb$month=="7" & sb$day %in% c("1","2","3","4","5","6","7","8","9","10"))] <- "2"


smaria$phen_stage_chard[which(smaria$month=="5" & smaria$day %in% c("6","7","8","9","10","11","12","13","14","15","16","17","18","19","20","21","22",
                                                                             "23","24","25","26","27","28","29","30","31"))] <- "2"
smaria$phen_stage_chard[which(smaria$month=="6")] <- "2"
smaria$phen_stage_chard[which(smaria$month=="7" & smaria$day %in% c("1","2","3","4","5","6","7","8","9","10"))] <- "2"

#Veraison (3)
sb$phen_stage_chard[which(sb$month=="7" & sb$day %in% c("11","12","13","14","15","16","17","18","19","20","21","22","23",
                                                                       "24","25","26","27","28","29","30","31"))] <- "3"
sb$phen_stage_chard[which(sb$month=="8")] <- "3"
sb$phen_stage_chard[which(sb$month=="9" & sb$day %in% c("1","2","3","4","5","6","7","8","9",
                                                                       "10","11","12","13","14","15"))] <- "3"

smaria$phen_stage_chard[which(smaria$month=="7" & smaria$day %in% c("11","12","13","14","15","16","17","18","19","20","21","22","23",
                                                                             "24","25","26","27","28","29","30","31"))] <- "3"
smaria$phen_stage_chard[which(smaria$month=="8")] <- "3"
smaria$phen_stage_chard[which(smaria$month=="9" & smaria$day %in% c("1","2","3","4","5","6","7","8","9",
                                                                             "10","11","12","13","14","15"))] <- "3"

#remove empty rows
sb <- sb[!(is.na(sb$phen_stage) & is.na(sb$phen_stage_chard)), ]
smaria <- smaria[!(is.na(smaria$phen_stage) & is.na(smaria$phen_stage_chard)), ]

#Calculating GDD at each location -- GDD base temp = 10
#Base GDD values - need individual per variety
#general
smaria$gddbase <- ifelse(!is.na(smaria$phen_stage) & smaria$TAVG >= 10, smaria$TAVG - 10, 0)
sb$gddbase <- ifelse(!is.na(sb$phen_stage) & sb$TAVG >= 10, sb$TAVG - 10, 0)
#chardonnay
smaria$gddbase_chard <- ifelse(!is.na(smaria$phen_stage_chard) & smaria$TAVG >= 10, smaria$TAVG - 10, 0)
sb$gddbase_chard <- ifelse(!is.na(sb$phen_stage_chard) & sb$TAVG >= 10, sb$TAVG - 10, 0)

#Summation by phenological stage and variety
#general
smaria$gdd_phen <- ave(smaria$gddbase, by = list(smaria$year, smaria$phen_stage), FUN = cumsum)
sb$gdd_phen <- ave(sb$gddbase, by = list(sb$year, sb$phen_stage), FUN = cumsum)
#chardonnay
smaria$gdd_phen_chard <- ave(smaria$gddbase_chard, by = list(smaria$year, smaria$phen_stage_chard), FUN = cumsum)
sb$gdd_phen_chard <- ave(sb$gddbase_chard, by = list(sb$year, sb$phen_stage_chard), FUN = cumsum)

#Summation by year and variety
#general
smaria$gdd_year <- ave(smaria$gddbase, smaria$year, FUN = cumsum)
sb$gdd_year <- ave(sb$gddbase, sb$year, FUN = cumsum)
#chardonnay
smaria$gdd_year_chard <- ave(smaria$gddbase_chard, smaria$year, FUN = cumsum)
sb$gdd_year_chard <- ave(sb$gddbase_chard, sb$year, FUN = cumsum)

#creating dataframe of agg gdd from each variety by year and phen stage at each location

#Phen Stage
#general
ag_smaria_phen_gen <- aggregate(smaria$gddbase, by = list(smaria$year, smaria$phen_stage), FUN=sum) 
colnames(ag_smaria_phen_gen)[1] <- c("Vintage")
colnames(ag_smaria_phen_gen)[2] <- c("phen_stage")
colnames(ag_smaria_phen_gen)[3] <- c("smaria_gdd")

ag_sb_phen_gen <- aggregate(sb$gddbase, by = list(sb$year, sb$phen_stage), FUN=sum) 
colnames(ag_sb_phen_gen)[1] <- c("Vintage")
colnames(ag_sb_phen_gen)[2] <- c("phen_stage")
colnames(ag_sb_phen_gen)[3] <- c("sb_gdd")

#chardonnay
ag_smaria_phen_chard <- aggregate(smaria$gddbase_chard, by = list(smaria$year, smaria$phen_stage_chard), FUN=sum) 
colnames(ag_smaria_phen_chard)[1] <- c("Vintage")
colnames(ag_smaria_phen_chard)[2] <- c("phen_stage")
colnames(ag_smaria_phen_chard)[3] <- c("smaria_gdd_chard")

ag_sb_phen_chard <- aggregate(sb$gddbase_chard, by = list(sb$year, sb$phen_stage_chard), FUN=sum) 
colnames(ag_sb_phen_chard)[1] <- c("Vintage")
colnames(ag_sb_phen_chard)[2] <- c("phen_stage")
colnames(ag_sb_phen_chard)[3] <- c("sb_gdd_chard")

gdd_agg_phen <- merge(ag_smaria_phen_gen, ag_sb_phen_gen)
gdd_agg_phen <- merge(gdd_agg_phen, ag_sb_phen_chard)
gdd_agg_phen <- merge(gdd_agg_phen, ag_smaria_phen_chard)

#averaging by location for each variety
gdd_agg_phen$gdd_avg_phen_gen <- rowMeans(gdd_agg_phen[, 3:4])
gdd_agg_phen$gdd_avg_phen_chard <- rowMeans(gdd_agg_phen[, 5:6])

#Year
#gen
ag_smaria_year_gen <- aggregate(smaria$gddbase, by = list(smaria$year), FUN=sum) 
colnames(ag_smaria_year_gen)[1] <- c("Vintage")
colnames(ag_smaria_year_gen)[2] <- c("smaria_gdd")

ag_sb_year_gen <- aggregate(sb$gddbase, by = list(sb$year), FUN=sum) 
colnames(ag_sb_year_gen)[1] <- c("Vintage")
colnames(ag_sb_year_gen)[2] <- c("sb_gdd")

#chardonnay
ag_smaria_year_chard <- aggregate(smaria$gddbase_chard, by = list(smaria$year), FUN=sum) 
colnames(ag_smaria_year_chard)[1] <- c("Vintage")
colnames(ag_smaria_year_chard)[2] <- c("smaria_gdd_chard")

ag_sb_year_chard <- aggregate(sb$gddbase_chard, by = list(sb$year), FUN=sum) 
colnames(ag_sb_year_chard)[1] <- c("Vintage")
colnames(ag_sb_year_chard)[2] <- c("sb_gdd_chard")

gdd_agg_year <- merge(ag_smaria_year_gen, ag_sb_year_gen)
gdd_agg_year <- merge(gdd_agg_year, ag_smaria_year_chard)
gdd_agg_year <- merge(gdd_agg_year, ag_sb_year_chard)

#averaging by location for each variety
gdd_agg_year$gdd_avg_year_gen <- rowMeans(gdd_agg_year[, 2:3])
gdd_agg_year$gdd_avg_year_chard <- rowMeans(gdd_agg_year[, 4:5])

#Calculating Precipitation at each location by year and phen stage

#Base GDD values - need individual per variety
#general
smaria$prcpbase_gen <- ifelse(is.na(smaria$phen_stage), 0, smaria$PRCP)
sb$prcpbase_gen <- ifelse(is.na(sb$phen_stage), 0, sb$PRCP)
#chardonnay
smaria$prcpbase_chard <- ifelse(is.na(smaria$phen_stage_chard), 0, smaria$PRCP)
sb$prcpbase_chard <- ifelse(is.na(sb$phen_stage_chard), 0, sb$PRCP)

#Summation by phenological stage and variety
#general
smaria$prcpsum_phen <- ave(smaria$prcpbase_gen, by = list(smaria$year, smaria$phen_stage), FUN = cumsum)
sb$prcpsum_phen <- ave(sb$prcpbase_gen, by = list(sb$year, sb$phen_stage), FUN = cumsum)
#chardonnay
smaria$prcpsum_phen_chard <- ave(smaria$prcpbase_chard, by = list(smaria$year, smaria$phen_stage_chard), FUN = cumsum)
sb$prcpsum_phen_chard <- ave(sb$prcpbase_chard, by = list(sb$year, sb$phen_stage_chard), FUN = cumsum)

#Summation by year and variety
#general
smaria$prcpsum_year <- ave(smaria$prcpbase_gen, smaria$year, FUN = cumsum)
sb$prcpsum_year <- ave(sb$prcpbase_gen, sb$year, FUN = cumsum)
#chardonnay
smaria$prcpsum_year_chard <- ave(smaria$prcpbase_chard, smaria$year, FUN = cumsum)
sb$prcpsum_year_chard <- ave(sb$prcpbase_chard, sb$year, FUN = cumsum)

#Creating dataframe of agg precipitation from each year and phen stage and each location

#By Phen Stage
#general
agp_smaria_phen_gen <- aggregate(smaria$prcpbase_gen, by = list(smaria$year, smaria$phen_stage), FUN=sum) 
colnames(agp_smaria_phen_gen)[1] <- c("Vintage")
colnames(agp_smaria_phen_gen)[2] <- c("phen_stage")
colnames(agp_smaria_phen_gen)[3] <- c("smaria_prcp")

agp_sb_phen_gen <- aggregate(sb$prcpbase_gen, by = list(sb$year, sb$phen_stage), FUN=sum) 
colnames(agp_sb_phen_gen)[1] <- c("Vintage")
colnames(agp_sb_phen_gen)[2] <- c("phen_stage")
colnames(agp_sb_phen_gen)[3] <- c("sb_prcp")

#chardonnay
agp_smaria_phen_chard <- aggregate(smaria$prcpbase_chard, by = list(smaria$year, smaria$phen_stage_chard), FUN=sum) 
colnames(agp_smaria_phen_chard)[1] <- c("Vintage")
colnames(agp_smaria_phen_chard)[2] <- c("phen_stage")
colnames(agp_smaria_phen_chard)[3] <- c("smaria_prcp_chard")

agp_sb_phen_chard <- aggregate(sb$prcpbase_chard, by = list(sb$year, sb$phen_stage_chard), FUN=sum) 
colnames(agp_sb_phen_chard)[1] <- c("Vintage")
colnames(agp_sb_phen_chard)[2] <- c("phen_stage")
colnames(agp_sb_phen_chard)[3] <- c("sb_prcp_chard")

prcp_agg_phen <- merge(agp_smaria_phen_gen, agp_sb_phen_gen)
prcp_agg_phen <- merge(prcp_agg_phen, agp_sb_phen_chard)
prcp_agg_phen <- merge(prcp_agg_phen, agp_smaria_phen_chard)

#averaging by location for each variety
prcp_agg_phen$prcp_avg_phen_gen <- rowMeans(prcp_agg_phen[, 3:4])
prcp_agg_phen$prcp_avg_phen_chard <- rowMeans(prcp_agg_phen[, 5:6])

#By Year
#gen
agp_smaria_year_gen <- aggregate(smaria$prcpbase_gen, by = list(smaria$year), FUN=sum) 
colnames(agp_smaria_year_gen)[1] <- c("Vintage")
colnames(agp_smaria_year_gen)[2] <- c("smaria_prcp")

agp_sb_year_gen <- aggregate(sb$prcpbase_gen, by = list(sb$year), FUN=sum) 
colnames(agp_sb_year_gen)[1] <- c("Vintage")
colnames(agp_sb_year_gen)[2] <- c("sb_prcp")

#chardonnay
agp_smaria_year_chard <- aggregate(smaria$prcpbase_chard, by = list(smaria$year), FUN=sum) 
colnames(agp_smaria_year_chard)[1] <- c("Vintage")
colnames(agp_smaria_year_chard)[2] <- c("smaria_prcp_chard")

agp_sb_year_chard <- aggregate(sb$prcpbase_chard, by = list(sb$year), FUN=sum) 
colnames(agp_sb_year_chard)[1] <- c("Vintage")
colnames(agp_sb_year_chard)[2] <- c("sb_prcp_chard")

prcp_agg_year <- merge(agp_smaria_year_gen, agp_sb_year_gen)
prcp_agg_year <- merge(prcp_agg_year, agp_smaria_year_chard)
prcp_agg_year <- merge(prcp_agg_year, agp_sb_year_chard)

#averaging by location for each variety
prcp_agg_year$prcp_avg_year_gen <- rowMeans(prcp_agg_year[, 2:3])
prcp_agg_year$prcp_avg_year_chard <- rowMeans(prcp_agg_year[, 4:5])

#Subsetting by Variety and location
unique(mydat$Variety)

#variety
Chardonnay <- subset(mydat, mydat$Variety=="Chardonnay")
Rhone <- subset(mydat, mydat$Variety=="Rhone")
Pinot <- subset(mydat, mydat$Variety=="Pinot Noir")
General <- subset(mydat, mydat$Variety=="General")

#creating 2 dataframes. One by year, one by phen. Both with gdd_agg, prcp_agg, vintage, and napa ratings
Agg_Table_year <- merge(prcp_agg_year, gdd_agg_year)
Agg_Table_phen <- merge(prcp_agg_phen, gdd_agg_phen)

#year
Pinot_Table <- merge(Agg_Table_year, Pinot) #values from gen
Pinot_Table <- Pinot_Table[,-c(2,3,4,5,7,8,9,10,11,13)] #keeping averages
colnames(Pinot_Table)[3] <- c("var_gdd_avg_year")
colnames(Pinot_Table)[2] <- c("var_prcp_avg_year")

Chardonnay_Table <- merge(Agg_Table_year, Chardonnay)
Chardonnay_Table <- Chardonnay_Table[,-c(2,3,4,5,6,8,9,10,11,12)] #keeping averages
colnames(Chardonnay_Table)[2] <- c("var_prcp_avg_year")
colnames(Chardonnay_Table)[3] <- c("var_gdd_avg_year")

Rhone_Table <- merge(Agg_Table_year, Rhone) #values from gen
Rhone_Table <- Rhone_Table[,-c(2,3,4,5,7,8,9,10,11,13)] #keeping averages
colnames(Rhone_Table)[2] <- c("var_prcp_avg_year")
colnames(Rhone_Table)[3] <- c("var_gdd_avg_year")

General_Table <- merge(Agg_Table_year, General) #values from gen
General_Table <- General_Table[,-c(2,3,4,5,7,8,9,10,11,13)] #keeping averages
colnames(General_Table)[2] <- c("var_prcp_avg_year")
colnames(General_Table)[3] <- c("var_gdd_avg_year")

SB_Table_Full_Year <- rbind(Pinot_Table, Chardonnay_Table, Rhone_Table, General_Table)
SB_Table_Full_Year <- SB_Table_Full_Year[,c(1,4,5,6,7,8,9,2,3,10,11)] #reorganize columns

#phen
Pinot_Table <- merge(Agg_Table_phen, Pinot) #values from gen
Pinot_Table <- Pinot_Table[,-c(3,4,5,6,8,9,10,11,12,14)] #keeping averages
colnames(Pinot_Table)[4] <- c("var_gdd_avg_phen")
colnames(Pinot_Table)[3] <- c("var_prcp_avg_phen")

Chardonnay_Table <- merge(Agg_Table_phen, Chardonnay)
Chardonnay_Table <- Chardonnay_Table[,-c(3,4,5,6,7,9,10,11,12,13)] #keeping averages
colnames(Chardonnay_Table)[4] <- c("var_gdd_avg_phen")
colnames(Chardonnay_Table)[3] <- c("var_prcp_avg_phen")

Rhone_Table <- merge(Agg_Table_phen, Rhone) #values from gen
Rhone_Table <- Rhone_Table[,-c(3,4,5,6,8,9,10,11,12,14)] #keeping averages
colnames(Rhone_Table)[4] <- c("var_gdd_avg_phen")
colnames(Rhone_Table)[3] <- c("var_prcp_avg_phen")

General_Table <- merge(Agg_Table_phen, General) #values from gen
General_Table <- General_Table[,-c(3,4,5,6,8,9,10,11,12,14)] #keeping averages
colnames(General_Table)[4] <- c("var_gdd_avg_phen")
colnames(General_Table)[3] <- c("var_prcp_avg_phen")

SB_Table_Full_Phen <- rbind(Pinot_Table, Chardonnay_Table, Rhone_Table, General_Table)
SB_Table_Full_Phen <- pivot_wider(SB_Table_Full_Phen, names_from = phen_stage, values_from = c(var_prcp_avg_phen, var_gdd_avg_phen))
SB_Table_Full_Phen <- SB_Table_Full_Phen[,c(1,2,3,4,5,6,7,10,11,12,13,14,15,8,9)] #reorganize columns

#Exporting as csv for future modeling
write.csv(SB_Table_Full_Year,"/Users/phoebeautio/Desktop/Vintages/data/NapaClimate2021/TablesForModels/SBComplete_year.csv", row.names = FALSE)
write.csv(SB_Table_Full_Phen,"/Users/phoebeautio/Desktop/Vintages/data/NapaClimate2021/TablesForModels/SBComplete_phen.csv", row.names = FALSE)
