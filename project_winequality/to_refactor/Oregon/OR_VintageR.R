################# Oregon Vintage Dataset (PA) - Updated 3/18/2021 #########################

#Housekeeping
rm(list=ls())
options(stringsAsFactors = FALSE)

#Load libraries
library(lubridate)
library(dplyr)
library(tidyr)

#Reading in csv files
mydat <- read.csv("/Users/phoebeautio/Desktop/Vintages/data/NapaClimate2021/Oregon/OR_Vintage.csv", header=TRUE, na.strings=c(""," ","NA"))
head(mydat)

sal_air <- read.csv("/Users/phoebeautio/Desktop/Vintages/data/NapaClimate2021/Oregon/SalAirClean.csv", header=TRUE, na.strings=c(""," ","NA"))
head(sal_air)

sil <- read.csv("/Users/phoebeautio/Desktop/Vintages/data/NapaClimate2021/Oregon/SilClean.csv", header=TRUE, na.strings=c(""," ","NA"))
head(sil)

## Relative paths (Geoff)
## mydat <- read.csv("OR_Vintage.csv", header=TRUE, na.strings=c(""," ","NA"))
## head(mydat)

#Adding Phenology Column (by Variety)
sil['phen_stage_pn'] <- NA
sal_air['phen_stage_pn'] <- NA

#General-Pinot
#Budburst (1)
sil$phen_stage_pn[which(sil$month=="4")] <- "1"
sil$phen_stage_pn[which(sil$month=="5" & sil$day %in% c("1","2","3","4","5","6","7","8","9","10","11","12","13","14","15"))] <- "1"

sal_air$phen_stage_pn[which(sal_air$month=="4")] <- "1"
sal_air$phen_stage_pn[which(sal_air$month=="5" & sal_air$day %in% c("1","2","3","4","5","6","7","8","9","10","11","12","13","14","15"))] <- "1"

#Bloom (2)
sil$phen_stage_pn[which(sil$month=="5" & sil$day %in% c("16","17","18","19","20","21","22",
                                                                 "23","24","25","26","27","28","29","30","31"))] <- "2"
sil$phen_stage_pn[which(sil$month=="6")] <- "2"
sil$phen_stage_pn[which(sil$month=="7" & sil$day %in% c("1","2","3","4","5","6","7","8","9",
                                                                 "10","11","12","13","14","15","16",
                                                                 "17","18","19","20","21","22","23"))] <- "2"
sal_air$phen_stage_pn[which(sal_air$month=="5" & sal_air$day %in% c("16","17","18","19","20","21","22",
                                                                       "23","24","25","26","27","28","29","30","31"))] <- "2"
sal_air$phen_stage_pn[which(sal_air$month=="6")] <- "2"
sal_air$phen_stage_pn[which(sal_air$month=="7" & sal_air$day %in% c("1","2","3","4","5","6","7","8","9",
                                                                       "10","11","12","13","14","15","16",
                                                                       "17","18","19","20","21","22","23"))] <- "2"
#Veraison (3)
sil$phen_stage_pn[which(sil$month=="7" & sil$day %in% c("24","25","26","27","28","29","30","31"))] <- "3"
sil$phen_stage_pn[which(sil$month=="8")] <- "3"
sil$phen_stage_pn[which(sil$month=="9" & sil$day %in% c("1","2","3","4","5","6","7","8","9",
                                                                 "10","11","12","13","14","15"))] <- "3"

sal_air$phen_stage_pn[which(sal_air$month=="7" & sal_air$day %in% c("24","25","26","27","28","29","30","31"))] <- "3"
sal_air$phen_stage_pn[which(sal_air$month=="8")] <- "3"
sal_air$phen_stage_pn[which(sal_air$month=="9" & sal_air$day %in% c("1","2","3","4","5","6","7","8","9",
                                                                       "10","11","12","13","14","15"))] <- "3"

#Calculating GDD -- GDD base temp = 10
#Base GDD values - need individual per variety
#pnoir
sal_air$gddbase_pn <- ifelse(!is.na(sal_air$phen_stage_pn) & sal_air$TAVG >= 10, sal_air$TAVG - 10, 0)
sil$gddbase_pn <- ifelse(!is.na(sil$phen_stage_pn) & sil$TAVG >= 10, sil$TAVG - 10, 0)

#Summation by phenological stage and variety
#Pinot
sal_air$gdd_phen_pn <- ave(sal_air$gddbase_pn, by = list(sal_air$year, sal_air$phen_stage_pn), FUN = cumsum)
sil$gdd_phen_pn <- ave(sil$gddbase_pn, by = list(sil$year, sil$phen_stage_pn), FUN = cumsum)

#Summation by year and variety
#Pinot
sal_air$gdd_year_pn <- ave(sal_air$gddbase_pn, sal_air$year, FUN = cumsum)
sil$gdd_year_pn <- ave(sil$gddbase_pn, sil$year, FUN = cumsum)

#creating dataframe of agg gdd, year and phen
#Phen Stage
#Pinot
ag_sal_air_phen_pn <- aggregate(sal_air$gddbase_pn, by = list(sal_air$year, sal_air$phen_stage_pn), FUN=sum) 
colnames(ag_sal_air_phen_pn)[1] <- c("Vintage")
colnames(ag_sal_air_phen_pn)[2] <- c("phen_stage")
colnames(ag_sal_air_phen_pn)[3] <- c("sal_air_gdd_pn")

ag_sil_phen_pn <- aggregate(sil$gddbase_pn, by = list(sil$year, sil$phen_stage_pn), FUN=sum) 
colnames(ag_sil_phen_pn)[1] <- c("Vintage")
colnames(ag_sil_phen_pn)[2] <- c("phen_stage")
colnames(ag_sil_phen_pn)[3] <- c("sil_gdd_pn")

gdd_agg_phen <- merge(ag_sal_air_phen_pn, ag_sil_phen_pn)

#averaging by location for each variety
gdd_agg_phen$gdd_avg_phen_pn <- rowMeans(gdd_agg_phen[, 3:4])

#Year
#Pinot
ag_sal_air_year_pn <- aggregate(sal_air$gddbase_pn, by = list(sal_air$year), FUN=sum) 
colnames(ag_sal_air_year_pn)[1] <- c("Vintage")
colnames(ag_sal_air_year_pn)[2] <- c("sal_air_gdd_pn")

ag_sil_year_pn <- aggregate(sil$gddbase_pn, by = list(sil$year), FUN=sum) 
colnames(ag_sil_year_pn)[1] <- c("Vintage")
colnames(ag_sil_year_pn)[2] <- c("sil_gdd_pn")

gdd_agg_year <- merge(ag_sal_air_year_pn, ag_sil_year_pn)

#averaging by location for each variety
gdd_agg_year$gdd_avg_year_pn <- rowMeans(gdd_agg_year[, 2:3])

#Calculating Precipitation at each location by year and phen stage

#Base GDD values - need individual per variety
#Pinot
sal_air$prcpbase_pn <- ifelse(is.na(sal_air$phen_stage_pn), 0, sal_air$PRCP)
sil$prcpbase_pn <- ifelse(is.na(sil$phen_stage_pn), 0, sil$PRCP)

#Summation by phenological stage and variety
#Pinot
sal_air$prcpsum_phen_pn <- ave(sal_air$prcpbase_pn, by = list(sal_air$year, sal_air$phen_stage_pn), FUN = cumsum)
sil$prcpsum_phen_pn <- ave(sil$prcpbase_pn, by = list(sil$year, sil$phen_stage_pn), FUN = cumsum)

#Summation by year and variety
#Pinot
sal_air$prcpsum_year_pn <- ave(sal_air$prcpbase_pn, sal_air$year, FUN = cumsum)
sil$prcpsum_year_pn <- ave(sil$prcpbase_pn, sil$year, FUN = cumsum)

#Creating dataframe of agg precipitation from each year and phen stage and each location

#By Phen Stage
#Pinot
agp_sal_air_phen_pn <- aggregate(sal_air$prcpbase_pn, by = list(sal_air$year, sal_air$phen_stage_pn), FUN=sum) 
colnames(agp_sal_air_phen_pn)[1] <- c("Vintage")
colnames(agp_sal_air_phen_pn)[2] <- c("phen_stage")
colnames(agp_sal_air_phen_pn)[3] <- c("sal_air_prcp_pn")

agp_sil_phen_pn <- aggregate(sil$prcpbase_pn, by = list(sil$year, sil$phen_stage_pn), FUN=sum) 
colnames(agp_sil_phen_pn)[1] <- c("Vintage")
colnames(agp_sil_phen_pn)[2] <- c("phen_stage")
colnames(agp_sil_phen_pn)[3] <- c("sil_prcp_pn")

prcp_agg_phen <- merge(agp_sal_air_phen_pn, agp_sil_phen_pn)

#averaging by location for each variety
prcp_agg_phen$prcp_avg_phen_pn <- rowMeans(prcp_agg_phen[, 3:4])

#By Year
#Pinot
agp_sal_air_year_pn <- aggregate(sal_air$prcpbase_pn, by = list(sal_air$year), FUN=sum) 
colnames(agp_sal_air_year_pn)[1] <- c("Vintage")
colnames(agp_sal_air_year_pn)[2] <- c("sal_air_prcp_pn")

agp_sil_year_pn <- aggregate(sil$prcpbase_pn, by = list(sil$year), FUN=sum) 
colnames(agp_sil_year_pn)[1] <- c("Vintage")
colnames(agp_sil_year_pn)[2] <- c("sil_prcp_pn")

prcp_agg_year <- merge(agp_sal_air_year_pn, agp_sil_year_pn)

#averaging by location for each variety
prcp_agg_year$prcp_avg_year_pn <- rowMeans(prcp_agg_year[, 2:3])

#Subsetting by Variety and location - only one
unique(mydat$Variety)

#creating 2 dataframes. One by year, one by phen. Both with gdd_agg, prcp_agg, vintage, and napa ratings
Agg_Table_year <- merge(prcp_agg_year, gdd_agg_year)
Agg_Table_phen <- merge(prcp_agg_phen, gdd_agg_phen)

#year
Pinot_Table <- merge(Agg_Table_year, mydat)
Pinot_Table <- Pinot_Table[,-c(2,3,5,6)] #keeping averages
colnames(Pinot_Table)[3] <- c("var_gdd_avg_year")
colnames(Pinot_Table)[2] <- c("var_prcp_avg_year")

OR_Table_Full_Year <- Pinot_Table[,c(1,4,5,6,7,8,9,2,3,10,11)] #reorganize columns

#phen
Pinot_Table <- merge(Agg_Table_phen, mydat)
Pinot_Table <- Pinot_Table[,-c(3,4,6,7)] #keeping averages
colnames(Pinot_Table)[4] <- c("var_gdd_avg_phen")
colnames(Pinot_Table)[3] <- c("var_prcp_avg_phen")

OR_Table_Full_Phen <- pivot_wider(Pinot_Table, names_from = phen_stage, values_from = c(var_prcp_avg_phen, var_gdd_avg_phen))
OR_Table_Full_Phen <- OR_Table_Full_Phen[,c(1,2,3,4,5,6,7,10,11,12,13,14,15,8,9)] #reorganize columns

#Exporting as csv for future modeling
write.csv(OR_Table_Full_Year,"/Users/phoebeautio/Desktop/Vintages/data/NapaClimate2021/TablesForModels/ORComplete_year.csv", row.names = FALSE)
write.csv(OR_Table_Full_Phen,"/Users/phoebeautio/Desktop/Vintages/data/NapaClimate2021/TablesForModels/ORComplete_phen.csv", row.names = FALSE)

