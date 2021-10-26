################# Washington Vintage Dataset (PA) - Updated 5/11/2021 #########################

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
wal <- read.csv("/Users/phoebeautio/Desktop/Vintages/data/NapaClimate2021/Washington/WalClean.csv", header=TRUE, na.strings=c(""," ","NA"))
head(hat)
eph <- read.csv("/Users/phoebeautio/Desktop/Vintages/data/NapaClimate2021/Washington/EphClean.csv", header=TRUE, na.strings=c(""," ","NA"))
head(hat)
dalle <- read.csv("/Users/phoebeautio/Desktop/Vintages/data/NapaClimate2021/Washington/DalleClean.csv", header=TRUE, na.strings=c(""," ","NA"))
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

wal['phen_stage_cab'] <- NA
wal['phen_stage_syrah'] <- NA
wal['phen_stage_mer'] <- NA

eph['phen_stage_cab'] <- NA
eph['phen_stage_syrah'] <- NA
eph['phen_stage_mer'] <- NA

dalle['phen_stage_cab'] <- NA
dalle['phen_stage_syrah'] <- NA
dalle['phen_stage_mer'] <- NA

#syrah (general)
#Budburst (1)
dayt$phen_stage_syrah[which(dayt$month=="4")] <- "1"
dayt$phen_stage_syrah[which(dayt$month=="5" & dayt$day %in% c("7","8","9","10","11","12","13","14","15"))] <- "1"

hat$phen_stage_syrah[which(hat$month=="4")] <- "1"
hat$phen_stage_syrah[which(hat$month=="5" & hat$day %in% c("1","2","3","4","5","6","7","8","9","10","11","12","13","14","15"))] <- "1"

wal$phen_stage_syrah[which(wal$month=="4")] <- "1"
wal$phen_stage_syrah[which(wal$month=="5" & wal$day %in% c("1","2","3","4","5","6","7","8","9","10","11","12","13","14","15"))] <- "1"

eph$phen_stage_syrah[which(eph$month=="4")] <- "1"
eph$phen_stage_syrah[which(eph$month=="5" & eph$day %in% c("1","2","3","4","5","6","7","8","9","10","11","12","13","14","15"))] <- "1"

dalle$phen_stage_syrah[which(dalle$month=="4")] <- "1"
dalle$phen_stage_syrah[which(dalle$month=="5" & dalle$day %in% c("1","2","3","4","5","6","7","8","9","10","11","12","13","14","15"))] <- "1"

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
wal$phen_stage_syrah[which(wal$month=="5" & wal$day %in% c("16","17","18","19","20","21","22",
                                                           "23","24","25","26","27","28","29","30","31"))] <- "2"
wal$phen_stage_syrah[which(wal$month=="6")] <- "2"
wal$phen_stage_syrah[which(wal$month=="7" & wal$day %in% c("1","2","3","4","5","6","7","8","9",
                                                           "10","11","12","13","14","15","16",
                                                           "17","18","19","20","21","22","23"))] <- "2"
eph$phen_stage_syrah[which(eph$month=="5" & eph$day %in% c("16","17","18","19","20","21","22",
                                                           "23","24","25","26","27","28","29","30","31"))] <- "2"
eph$phen_stage_syrah[which(eph$month=="6")] <- "2"
eph$phen_stage_syrah[which(eph$month=="7" & eph$day %in% c("1","2","3","4","5","6","7","8","9",
                                                           "10","11","12","13","14","15","16",
                                                           "17","18","19","20","21","22","23"))] <- "2"
dalle$phen_stage_syrah[which(dalle$month=="5" & dalle$day %in% c("16","17","18","19","20","21","22",
                                                           "23","24","25","26","27","28","29","30","31"))] <- "2"
dalle$phen_stage_syrah[which(dalle$month=="6")] <- "2"
dalle$phen_stage_syrah[which(dalle$month=="7" & dalle$day %in% c("1","2","3","4","5","6","7","8","9",
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
wal$phen_stage_syrah[which(wal$month=="7" & wal$day %in% c("24","25","26","27","28","29","30","31"))] <- "3"
wal$phen_stage_syrah[which(wal$month=="8")] <- "3"
wal$phen_stage_syrah[which(wal$month=="9" & wal$day %in% c("1","2","3","4","5","6","7","8","9",
                                                           "10","11","12","13","14","15"))] <- "3"
eph$phen_stage_syrah[which(eph$month=="7" & eph$day %in% c("24","25","26","27","28","29","30","31"))] <- "3"
eph$phen_stage_syrah[which(eph$month=="8")] <- "3"
eph$phen_stage_syrah[which(eph$month=="9" & eph$day %in% c("1","2","3","4","5","6","7","8","9",
                                                           "10","11","12","13","14","15"))] <- "3"
dalle$phen_stage_syrah[which(dalle$month=="7" & dalle$day %in% c("24","25","26","27","28","29","30","31"))] <- "3"
dalle$phen_stage_syrah[which(dalle$month=="8")] <- "3"
dalle$phen_stage_syrah[which(dalle$month=="9" & dalle$day %in% c("1","2","3","4","5","6","7","8","9",
                                                           "10","11","12","13","14","15"))] <- "3"

#Cabernet (general)
dayt$phen_stage_cab[which(dayt$month=="4")] <- "1"
dayt$phen_stage_cab[which(dayt$month=="5" & dayt$day %in% c("7","8","9","10","11","12","13","14","15"))] <- "1"

hat$phen_stage_cab[which(hat$month=="4")] <- "1"
hat$phen_stage_cab[which(hat$month=="5" & hat$day %in% c("1","2","3","4","5","6","7","8","9","10","11","12","13","14","15"))] <- "1"

wal$phen_stage_cab[which(wal$month=="4")] <- "1"
wal$phen_stage_cab[which(wal$month=="5" & wal$day %in% c("1","2","3","4","5","6","7","8","9","10","11","12","13","14","15"))] <- "1"

eph$phen_stage_cab[which(eph$month=="4")] <- "1"
eph$phen_stage_cab[which(eph$month=="5" & eph$day %in% c("1","2","3","4","5","6","7","8","9","10","11","12","13","14","15"))] <- "1"

dalle$phen_stage_cab[which(dalle$month=="4")] <- "1"
dalle$phen_stage_cab[which(dalle$month=="5" & dalle$day %in% c("1","2","3","4","5","6","7","8","9","10","11","12","13","14","15"))] <- "1"

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
wal$phen_stage_cab[which(wal$month=="5" & wal$day %in% c("16","17","18","19","20","21","22",
                                                           "23","24","25","26","27","28","29","30","31"))] <- "2"
wal$phen_stage_cab[which(wal$month=="6")] <- "2"
wal$phen_stage_cab[which(wal$month=="7" & wal$day %in% c("1","2","3","4","5","6","7","8","9",
                                                           "10","11","12","13","14","15","16",
                                                           "17","18","19","20","21","22","23"))] <- "2"
eph$phen_stage_cab[which(eph$month=="5" & eph$day %in% c("16","17","18","19","20","21","22",
                                                           "23","24","25","26","27","28","29","30","31"))] <- "2"
eph$phen_stage_cab[which(eph$month=="6")] <- "2"
eph$phen_stage_cab[which(eph$month=="7" & eph$day %in% c("1","2","3","4","5","6","7","8","9",
                                                           "10","11","12","13","14","15","16",
                                                           "17","18","19","20","21","22","23"))] <- "2"
dalle$phen_stage_cab[which(dalle$month=="5" & dalle$day %in% c("16","17","18","19","20","21","22",
                                                                 "23","24","25","26","27","28","29","30","31"))] <- "2"
dalle$phen_stage_cab[which(dalle$month=="6")] <- "2"
dalle$phen_stage_cab[which(dalle$month=="7" & dalle$day %in% c("1","2","3","4","5","6","7","8","9",
                                                                 "10","11","12","13","14","15","16",
                                                                 "17","18","19","20","21","22","23"))] <- "2"
#Veraison (3)
dayt$phen_stage_cab[which(dayt$month=="7" & dayt$day %in% c("24","25","26","27","28","29","30","31"))] <- "3"
dayt$phen_stage_cab[which(dayt$month=="8")] <- "3"
dayt$phen_stage_cab[which(dayt$month=="9" & dayt$day %in% c("1","2","3","4","5","6","7","8","9",
                                                              "10","11","12","13","14","15"))] <- "3"
hat$phen_stage_cab[which(hat$month=="7" & hat$day %in% c("24","25","26","27","28","29","30","31"))] <- "3"
hat$phen_stage_cab[which(hat$month=="8")] <- "3"
hat$phen_stage_cab[which(hat$month=="9" & hat$day %in% c("1","2","3","4","5","6","7","8","9",
                                                           "10","11","12","13","14","15"))] <- "3"
wal$phen_stage_cab[which(wal$month=="7" & wal$day %in% c("24","25","26","27","28","29","30","31"))] <- "3"
wal$phen_stage_cab[which(wal$month=="8")] <- "3"
wal$phen_stage_cab[which(wal$month=="9" & wal$day %in% c("1","2","3","4","5","6","7","8","9",
                                                           "10","11","12","13","14","15"))] <- "3"
eph$phen_stage_cab[which(eph$month=="7" & eph$day %in% c("24","25","26","27","28","29","30","31"))] <- "3"
eph$phen_stage_cab[which(eph$month=="8")] <- "3"
eph$phen_stage_cab[which(eph$month=="9" & eph$day %in% c("1","2","3","4","5","6","7","8","9",
                                                           "10","11","12","13","14","15"))] <- "3"
dalle$phen_stage_cab[which(dalle$month=="7" & dalle$day %in% c("24","25","26","27","28","29","30","31"))] <- "3"
dalle$phen_stage_cab[which(dalle$month=="8")] <- "3"
dalle$phen_stage_cab[which(dalle$month=="9" & dalle$day %in% c("1","2","3","4","5","6","7","8","9",
                                                                 "10","11","12","13","14","15"))] <- "3"
#Merlot
#Budburst (1)
dayt$phen_stage_mer[which(dayt$month=="3" & dayt$day %in% c("25","26","27","28","29","30","31"))] <- "1"
dayt$phen_stage_mer[which(dayt$month=="4")] <- "1"
dayt$phen_stage_mer[which(dayt$month=="5" & dayt$day %in% c("1","2","3","4","5","6","7","8","9","10","11","12","13"))] <- "1"

hat$phen_stage_mer[which(hat$month=="3" & hat$day %in% c("25","26","27","28","29","30","31"))] <- "1"
hat$phen_stage_mer[which(hat$month=="4")] <- "1"
hat$phen_stage_mer[which(hat$month=="5" & hat$day %in% c("1","2","3","4","5","6","7","8","9","10","11","12","13"))] <- "1"

wal$phen_stage_mer[which(wal$month=="3" & wal$day %in% c("25","26","27","28","29","30","31"))] <- "1"
wal$phen_stage_mer[which(wal$month=="4")] <- "1"
wal$phen_stage_mer[which(wal$month=="5" & wal$day %in% c("1","2","3","4","5","6","7","8","9","10","11","12","13"))] <- "1"

eph$phen_stage_mer[which(eph$month=="3" & eph$day %in% c("25","26","27","28","29","30","31"))] <- "1"
eph$phen_stage_mer[which(eph$month=="4")] <- "1"
eph$phen_stage_mer[which(eph$month=="5" & eph$day %in% c("1","2","3","4","5","6","7","8","9","10","11","12","13"))] <- "1"

dalle$phen_stage_mer[which(dalle$month=="3" & dalle$day %in% c("25","26","27","28","29","30","31"))] <- "1"
dalle$phen_stage_mer[which(dalle$month=="4")] <- "1"
dalle$phen_stage_mer[which(dalle$month=="5" & dalle$day %in% c("1","2","3","4","5","6","7","8","9","10","11","12","13"))] <- "1"

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

wal$phen_stage_mer[which(wal$month=="5" & wal$day %in% c("14","15","16","17","18","19","20","21","22",
                                                         "23","24","25","26","27","28","29","30","31"))] <- "2"
wal$phen_stage_mer[which(wal$month=="6")] <- "2"
wal$phen_stage_mer[which(wal$month=="7" & wal$day %in% c("1","2","3","4","5","6","7","8","9",
                                                         "10","11","12","13","14","15"))] <- "2"

eph$phen_stage_mer[which(eph$month=="5" & eph$day %in% c("14","15","16","17","18","19","20","21","22",
                                                         "23","24","25","26","27","28","29","30","31"))] <- "2"
eph$phen_stage_mer[which(eph$month=="6")] <- "2"
eph$phen_stage_mer[which(eph$month=="7" & eph$day %in% c("1","2","3","4","5","6","7","8","9",
                                                         "10","11","12","13","14","15"))] <- "2"

dalle$phen_stage_mer[which(dalle$month=="5" & dalle$day %in% c("14","15","16","17","18","19","20","21","22",
                                                         "23","24","25","26","27","28","29","30","31"))] <- "2"
dalle$phen_stage_mer[which(dalle$month=="6")] <- "2"
dalle$phen_stage_mer[which(dalle$month=="7" & dalle$day %in% c("1","2","3","4","5","6","7","8","9",
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

wal$phen_stage_mer[which(wal$month=="7" & wal$day %in% c("16","17","18","19","20","21","22","23","24","25",
                                                         "26","27","28","29","30","31"))] <- "3"
wal$phen_stage_mer[which(wal$month=="8")] <- "3"
wal$phen_stage_mer[which(wal$month=="9" & wal$day %in% c("1","2","3","4","5","6","7","8","9",
                                                         "10","11","12","13","14","15"))] <- "3"

eph$phen_stage_mer[which(eph$month=="7" & eph$day %in% c("16","17","18","19","20","21","22","23","24","25",
                                                         "26","27","28","29","30","31"))] <- "3"
eph$phen_stage_mer[which(eph$month=="8")] <- "3"
eph$phen_stage_mer[which(eph$month=="9" & eph$day %in% c("1","2","3","4","5","6","7","8","9",
                                                         "10","11","12","13","14","15"))] <- "3"

dalle$phen_stage_mer[which(dalle$month=="7" & dalle$day %in% c("16","17","18","19","20","21","22","23","24","25",
                                                         "26","27","28","29","30","31"))] <- "3"
dalle$phen_stage_mer[which(dalle$month=="8")] <- "3"
dalle$phen_stage_mer[which(dalle$month=="9" & dalle$day %in% c("1","2","3","4","5","6","7","8","9",
                                                         "10","11","12","13","14","15"))] <- "3"
#remove empty rows
dayt <- dayt[!(is.na(dayt$phen_stage_cab) & is.na(dayt$phen_stage_syrah) & is.na(dayt$phen_stage_mer)), ]
hat <- hat[!(is.na(hat$phen_stage_cab) & is.na(hat$phen_stage_syrah) & is.na(hat$phen_stage_mer)), ]
wal <- wal[!(is.na(wal$phen_stage_cab) & is.na(wal$phen_stage_syrah) & is.na(wal$phen_stage_mer)), ]
eph <- eph[!(is.na(eph$phen_stage_cab) & is.na(eph$phen_stage_syrah) & is.na(eph$phen_stage_mer)), ]
dalle <- dalle[!(is.na(dalle$phen_stage_cab) & is.na(dalle$phen_stage_syrah) & is.na(dalle$phen_stage_mer)), ]

#Calculating GDD at each location -- GDD base temp = 10
#Base GDD values - need individual per variety
#syrah
hat$gddbase_syrah <- ifelse(!is.na(hat$phen_stage_syrah) & hat$TAVG >= 10, hat$TAVG - 10, 0)
dayt$gddbase_syrah <- ifelse(!is.na(dayt$phen_stage_syrah) & dayt$TAVG >= 10, dayt$TAVG - 10, 0)
wal$gddbase_syrah <- ifelse(!is.na(wal$phen_stage_syrah) & wal$TAVG >= 10, wal$TAVG - 10, 0)
eph$gddbase_syrah <- ifelse(!is.na(eph$phen_stage_syrah) & eph$TAVG >= 10, eph$TAVG - 10, 0)
dalle$gddbase_syrah <- ifelse(!is.na(dalle$phen_stage_syrah) & dalle$TAVG >= 10, dalle$TAVG - 10, 0)
#cabernet
hat$gddbase_cab <- ifelse(!is.na(hat$phen_stage_cab) & hat$TAVG >= 10, hat$TAVG - 10, 0)
dayt$gddbase_cab <- ifelse(!is.na(dayt$phen_stage_cab) & dayt$TAVG >= 10, dayt$TAVG - 10, 0)
wal$gddbase_cab <- ifelse(!is.na(wal$phen_stage_cab) & wal$TAVG >= 10, wal$TAVG - 10, 0)
eph$gddbase_cab <- ifelse(!is.na(eph$phen_stage_cab) & eph$TAVG >= 10, eph$TAVG - 10, 0)
dalle$gddbase_cab <- ifelse(!is.na(dalle$phen_stage_cab) & dalle$TAVG >= 10, dalle$TAVG - 10, 0)
#merlot
hat$gddbase_mer <- ifelse(!is.na(hat$phen_stage_mer) & hat$TAVG >= 10, hat$TAVG - 10, 0)
dayt$gddbase_mer <- ifelse(!is.na(dayt$phen_stage_mer) & dayt$TAVG >= 10, dayt$TAVG - 10, 0)
wal$gddbase_mer <- ifelse(!is.na(wal$phen_stage_mer) & wal$TAVG >= 10, wal$TAVG - 10, 0)
eph$gddbase_mer <- ifelse(!is.na(eph$phen_stage_mer) & eph$TAVG >= 10, eph$TAVG - 10, 0)
dalle$gddbase_mer <- ifelse(!is.na(dalle$phen_stage_mer) & dalle$TAVG >= 10, dalle$TAVG - 10, 0)

#Summation by phenological stage and variety
#merlot
hat$gdd_phen_mer <- ave(hat$gddbase_mer, by = list(hat$year, hat$phen_stage_mer), FUN = cumsum)
dayt$gdd_phen_mer <- ave(dayt$gddbase_mer, by = list(dayt$year, dayt$phen_stage_mer), FUN = cumsum)
wal$gdd_phen_mer <- ave(wal$gddbase_mer, by = list(wal$year, wal$phen_stage_mer), FUN = cumsum)
eph$gdd_phen_mer <- ave(eph$gddbase_mer, by = list(eph$year, eph$phen_stage_mer), FUN = cumsum)
dalle$gdd_phen_mer <- ave(dalle$gddbase_mer, by = list(dalle$year, dalle$phen_stage_mer), FUN = cumsum)
#syrah
hat$gdd_phen_syrah <- ave(hat$gddbase_syrah, by = list(hat$year, hat$phen_stage_syrah), FUN = cumsum)
dayt$gdd_phen_syrah <- ave(dayt$gddbase_syrah, by = list(dayt$year, dayt$phen_stage_syrah), FUN = cumsum)
wal$gdd_phen_syrah <- ave(wal$gddbase_syrah, by = list(wal$year, wal$phen_stage_syrah), FUN = cumsum)
eph$gdd_phen_syrah <- ave(eph$gddbase_syrah, by = list(eph$year, eph$phen_stage_syrah), FUN = cumsum)
dalle$gdd_phen_syrah <- ave(dalle$gddbase_syrah, by = list(dalle$year, dalle$phen_stage_syrah), FUN = cumsum)
#cabernet
hat$gdd_phen_cab <- ave(hat$gddbase_cab, by = list(hat$year, hat$phen_stage_cab), FUN = cumsum)
dayt$gdd_phen_cab <- ave(dayt$gddbase_cab, by = list(dayt$year, dayt$phen_stage_cab), FUN = cumsum)
wal$gdd_phen_cab <- ave(wal$gddbase_cab, by = list(wal$year, wal$phen_stage_cab), FUN = cumsum)
eph$gdd_phen_cab <- ave(eph$gddbase_cab, by = list(eph$year, eph$phen_stage_cab), FUN = cumsum)
dalle$gdd_phen_cab <- ave(dalle$gddbase_cab, by = list(dalle$year, dalle$phen_stage_cab), FUN = cumsum)

#Summation by year and variety
#merlot
hat$gdd_year_mer <- ave(hat$gddbase_mer, hat$year, FUN = cumsum)
dayt$gdd_year_mer <- ave(dayt$gddbase_mer, dayt$year, FUN = cumsum)
wal$gdd_year_mer <- ave(wal$gddbase_mer, wal$year, FUN = cumsum)
eph$gdd_year_mer <- ave(eph$gddbase_mer, eph$year, FUN = cumsum)
dalle$gdd_year_mer <- ave(dalle$gddbase_mer, dalle$year, FUN = cumsum)
#syrah
hat$gdd_year_syrah <- ave(hat$gddbase_syrah, hat$year, FUN = cumsum)
dayt$gdd_year_syrah <- ave(dayt$gddbase_syrah, dayt$year, FUN = cumsum)
wal$gdd_year_syrah <- ave(wal$gddbase_syrah, wal$year, FUN = cumsum)
eph$gdd_year_syrah <- ave(eph$gddbase_syrah, eph$year, FUN = cumsum)
dalle$gdd_year_syrah <- ave(dalle$gddbase_syrah, dalle$year, FUN = cumsum)
#cabernet
hat$gdd_year_cab <- ave(hat$gddbase_cab, hat$year, FUN = cumsum)
dayt$gdd_year_cab <- ave(dayt$gddbase_cab, dayt$year, FUN = cumsum)
wal$gdd_year_cab <- ave(wal$gddbase_cab, wal$year, FUN = cumsum)
eph$gdd_year_cab <- ave(eph$gddbase_cab, eph$year, FUN = cumsum)
dalle$gdd_year_cab <- ave(dalle$gddbase_cab, dalle$year, FUN = cumsum)

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

ag_wal_phen_cab <- aggregate(wal$gddbase_cab, by = list(wal$year, wal$phen_stage_cab), FUN=sum) 
colnames(ag_wal_phen_cab)[1] <- c("Vintage")
colnames(ag_wal_phen_cab)[2] <- c("phen_stage")
colnames(ag_wal_phen_cab)[3] <- c("wal_gdd_cab")

ag_eph_phen_cab <- aggregate(eph$gddbase_cab, by = list(eph$year, eph$phen_stage_cab), FUN=sum) 
colnames(ag_eph_phen_cab)[1] <- c("Vintage")
colnames(ag_eph_phen_cab)[2] <- c("phen_stage")
colnames(ag_eph_phen_cab)[3] <- c("eph_gdd_cab")

ag_dalle_phen_cab <- aggregate(dalle$gddbase_cab, by = list(dalle$year, dalle$phen_stage_cab), FUN=sum) 
colnames(ag_dalle_phen_cab)[1] <- c("Vintage")
colnames(ag_dalle_phen_cab)[2] <- c("phen_stage")
colnames(ag_dalle_phen_cab)[3] <- c("dalle_gdd_cab")

#syrah
ag_hat_phen_syrah <- aggregate(hat$gddbase_syrah, by = list(hat$year, hat$phen_stage_syrah), FUN=sum) 
colnames(ag_hat_phen_syrah)[1] <- c("Vintage")
colnames(ag_hat_phen_syrah)[2] <- c("phen_stage")
colnames(ag_hat_phen_syrah)[3] <- c("hat_gdd_syrah")

ag_dayt_phen_syrah <- aggregate(dayt$gddbase_syrah, by = list(dayt$year, dayt$phen_stage_syrah), FUN=sum) 
colnames(ag_dayt_phen_syrah)[1] <- c("Vintage")
colnames(ag_dayt_phen_syrah)[2] <- c("phen_stage")
colnames(ag_dayt_phen_syrah)[3] <- c("dayt_gdd_syrah")

ag_wal_phen_syrah <- aggregate(wal$gddbase_syrah, by = list(wal$year, wal$phen_stage_syrah), FUN=sum) 
colnames(ag_wal_phen_syrah)[1] <- c("Vintage")
colnames(ag_wal_phen_syrah)[2] <- c("phen_stage")
colnames(ag_wal_phen_syrah)[3] <- c("wal_gdd_syrah")

ag_eph_phen_syrah <- aggregate(eph$gddbase_syrah, by = list(eph$year, eph$phen_stage_syrah), FUN=sum) 
colnames(ag_eph_phen_syrah)[1] <- c("Vintage")
colnames(ag_eph_phen_syrah)[2] <- c("phen_stage")
colnames(ag_eph_phen_syrah)[3] <- c("eph_gdd_syrah")

ag_dalle_phen_syrah <- aggregate(dalle$gddbase_syrah, by = list(dalle$year, dalle$phen_stage_syrah), FUN=sum) 
colnames(ag_dalle_phen_syrah)[1] <- c("Vintage")
colnames(ag_dalle_phen_syrah)[2] <- c("phen_stage")
colnames(ag_dalle_phen_syrah)[3] <- c("dalle_gdd_syrah")

#merlot
ag_hat_phen_mer <- aggregate(hat$gddbase_mer, by = list(hat$year, hat$phen_stage_mer), FUN=sum) 
colnames(ag_hat_phen_mer)[1] <- c("Vintage")
colnames(ag_hat_phen_mer)[2] <- c("phen_stage")
colnames(ag_hat_phen_mer)[3] <- c("hat_gdd_mer")

ag_dayt_phen_mer <- aggregate(dayt$gddbase_mer, by = list(dayt$year, dayt$phen_stage_mer), FUN=sum) 
colnames(ag_dayt_phen_mer)[1] <- c("Vintage")
colnames(ag_dayt_phen_mer)[2] <- c("phen_stage")
colnames(ag_dayt_phen_mer)[3] <- c("dayt_gdd_mer")

ag_wal_phen_mer <- aggregate(wal$gddbase_mer, by = list(wal$year, wal$phen_stage_mer), FUN=sum) 
colnames(ag_wal_phen_mer)[1] <- c("Vintage")
colnames(ag_wal_phen_mer)[2] <- c("phen_stage")
colnames(ag_wal_phen_mer)[3] <- c("wal_gdd_mer")

ag_eph_phen_mer <- aggregate(eph$gddbase_mer, by = list(eph$year, eph$phen_stage_mer), FUN=sum) 
colnames(ag_eph_phen_mer)[1] <- c("Vintage")
colnames(ag_eph_phen_mer)[2] <- c("phen_stage")
colnames(ag_eph_phen_mer)[3] <- c("eph_gdd_mer")

ag_dalle_phen_mer <- aggregate(dalle$gddbase_mer, by = list(dalle$year, dalle$phen_stage_mer), FUN=sum) 
colnames(ag_dalle_phen_mer)[1] <- c("Vintage")
colnames(ag_dalle_phen_mer)[2] <- c("phen_stage")
colnames(ag_dalle_phen_mer)[3] <- c("dalle_gdd_mer")

#editing stations with missing years
#ag_dalle_phen_cab
dalle2.16 <- data.frame(Vintage = "2016", phen_stage = "2", dalle_gdd_cab = NA)
dalle3.16 <- data.frame(Vintage = "2016", phen_stage = "3", dalle_gdd_cab = NA)
dalle1.17 <- data.frame(Vintage = "2017", phen_stage = "1", dalle_gdd_cab = NA)
dalle2.17 <- data.frame(Vintage = "2017", phen_stage = "2", dalle_gdd_cab = NA)
dalle3.17 <- data.frame(Vintage = "2017", phen_stage = "3", dalle_gdd_cab = NA)
dalle1.18 <- data.frame(Vintage = "2018", phen_stage = "1", dalle_gdd_cab = NA)
dalle2.18 <- data.frame(Vintage = "2018", phen_stage = "2", dalle_gdd_cab = NA)
dalle3.18 <- data.frame(Vintage = "2018", phen_stage = "3", dalle_gdd_cab = NA)

ag_dalle_phen_cab = rbind(ag_dalle_phen_cab, dalle2.16, dalle3.16, dalle1.17, dalle2.17, dalle3.17, dalle1.18, dalle2.18, dalle3.18)
ag_dalle_phen_cab <- ag_dalle_phen_cab[order(ag_dalle_phen_cab$Vintage), ]

#ag_dalle_phen_syrah
dalle2.16 <- data.frame(Vintage = "2016", phen_stage = "2", dalle_gdd_syrah = NA)
dalle3.16 <- data.frame(Vintage = "2016", phen_stage = "3", dalle_gdd_syrah = NA)
dalle1.17 <- data.frame(Vintage = "2017", phen_stage = "1", dalle_gdd_syrah = NA)
dalle2.17 <- data.frame(Vintage = "2017", phen_stage = "2", dalle_gdd_syrah = NA)
dalle3.17 <- data.frame(Vintage = "2017", phen_stage = "3", dalle_gdd_syrah = NA)
dalle1.18 <- data.frame(Vintage = "2018", phen_stage = "1", dalle_gdd_syrah = NA)
dalle2.18 <- data.frame(Vintage = "2018", phen_stage = "2", dalle_gdd_syrah = NA)
dalle3.18 <- data.frame(Vintage = "2018", phen_stage = "3", dalle_gdd_syrah = NA)

ag_dalle_phen_syrah = rbind(ag_dalle_phen_syrah, dalle2.16, dalle3.16, dalle1.17, dalle2.17, dalle3.17, dalle1.18, dalle2.18, dalle3.18)
ag_dalle_phen_syrah <- ag_dalle_phen_syrah[order(ag_dalle_phen_syrah$Vintage), ]

#ag_dalle_phen_mer
dalle2.16 <- data.frame(Vintage = "2016", phen_stage = "2", dalle_gdd_mer = NA)
dalle3.16 <- data.frame(Vintage = "2016", phen_stage = "3", dalle_gdd_mer = NA)
dalle1.17 <- data.frame(Vintage = "2017", phen_stage = "1", dalle_gdd_mer = NA)
dalle2.17 <- data.frame(Vintage = "2017", phen_stage = "2", dalle_gdd_mer = NA)
dalle3.17 <- data.frame(Vintage = "2017", phen_stage = "3", dalle_gdd_mer = NA)
dalle1.18 <- data.frame(Vintage = "2018", phen_stage = "1", dalle_gdd_mer = NA)
dalle2.18 <- data.frame(Vintage = "2018", phen_stage = "2", dalle_gdd_mer = NA)
dalle3.18 <- data.frame(Vintage = "2018", phen_stage = "3", dalle_gdd_mer = NA)

ag_dalle_phen_mer = rbind(ag_dalle_phen_mer, dalle2.16, dalle3.16, dalle1.17, dalle2.17, dalle3.17, dalle1.18, dalle2.18, dalle3.18)
ag_dalle_phen_mer <- ag_dalle_phen_mer[order(ag_dalle_phen_mer$Vintage), ]

#ag_eph_phen_cab
eph1.16 <- data.frame(Vintage = "2016", phen_stage = "1", eph_gdd_cab = NA)
eph2.16 <- data.frame(Vintage = "2016", phen_stage = "2", eph_gdd_cab = NA)
eph3.16 <- data.frame(Vintage = "2016", phen_stage = "3", eph_gdd_cab = NA)
eph1.17 <- data.frame(Vintage = "2017", phen_stage = "1", eph_gdd_cab = NA)
eph2.17 <- data.frame(Vintage = "2017", phen_stage = "2", eph_gdd_cab = NA)
eph3.17 <- data.frame(Vintage = "2017", phen_stage = "3", eph_gdd_cab = NA)
eph1.18 <- data.frame(Vintage = "2018", phen_stage = "1", eph_gdd_cab = NA)
eph2.18 <- data.frame(Vintage = "2018", phen_stage = "2", eph_gdd_cab = NA)
eph3.18 <- data.frame(Vintage = "2018", phen_stage = "3", eph_gdd_cab = NA)

ag_eph_phen_cab = rbind(ag_eph_phen_cab, eph1.16, eph2.16, eph3.16, eph1.17, eph2.17, eph3.17, eph1.18, eph2.18, eph3.18)
ag_eph_phen_cab <- ag_eph_phen_cab[order(ag_eph_phen_cab$Vintage), ]

#ag_eph_phen_syrah
eph1.16 <- data.frame(Vintage = "2016", phen_stage = "1", eph_gdd_syrah = NA)
eph2.16 <- data.frame(Vintage = "2016", phen_stage = "2", eph_gdd_syrah = NA)
eph3.16 <- data.frame(Vintage = "2016", phen_stage = "3", eph_gdd_syrah = NA)
eph1.17 <- data.frame(Vintage = "2017", phen_stage = "1", eph_gdd_syrah = NA)
eph2.17 <- data.frame(Vintage = "2017", phen_stage = "2", eph_gdd_syrah = NA)
eph3.17 <- data.frame(Vintage = "2017", phen_stage = "3", eph_gdd_syrah = NA)
eph1.18 <- data.frame(Vintage = "2018", phen_stage = "1", eph_gdd_syrah = NA)
eph2.18 <- data.frame(Vintage = "2018", phen_stage = "2", eph_gdd_syrah = NA)
eph3.18 <- data.frame(Vintage = "2018", phen_stage = "3", eph_gdd_syrah = NA)

ag_eph_phen_syrah = rbind(ag_eph_phen_syrah, eph1.16, eph2.16, eph3.16, eph1.17, eph2.17, eph3.17, eph1.18, eph2.18, eph3.18)
ag_eph_phen_syrah <- ag_eph_phen_syrah[order(ag_eph_phen_syrah$Vintage), ]

#ag_eph_phen_mer
eph2.16 <- data.frame(Vintage = "2016", phen_stage = "2", eph_gdd_mer = NA)
eph3.16 <- data.frame(Vintage = "2016", phen_stage = "3", eph_gdd_mer = NA)
eph1.17 <- data.frame(Vintage = "2017", phen_stage = "1", eph_gdd_mer = NA)
eph2.17 <- data.frame(Vintage = "2017", phen_stage = "2", eph_gdd_mer = NA)
eph3.17 <- data.frame(Vintage = "2017", phen_stage = "3", eph_gdd_mer = NA)
eph1.18 <- data.frame(Vintage = "2018", phen_stage = "1", eph_gdd_mer = NA)
eph2.18 <- data.frame(Vintage = "2018", phen_stage = "2", eph_gdd_mer = NA)
eph3.18 <- data.frame(Vintage = "2018", phen_stage = "3", eph_gdd_mer = NA)

ag_eph_phen_mer = rbind(ag_eph_phen_mer, eph2.16, eph3.16, eph1.17, eph2.17, eph3.17, eph1.18, eph2.18, eph3.18)
ag_eph_phen_mer <- ag_eph_phen_mer[order(ag_eph_phen_mer$Vintage), ]

#ag_wal_phen_cab
wal1.95 <- data.frame(Vintage = "1995", phen_stage = "1", wal_gdd_cab = NA)
wal1.96 <- data.frame(Vintage = "1996", phen_stage = "1", wal_gdd_cab = NA)
wal1.97 <- data.frame(Vintage = "1997", phen_stage = "1", wal_gdd_cab = NA)
wal1.98 <- data.frame(Vintage = "1998", phen_stage = "1", wal_gdd_cab = NA)
wal2.95 <- data.frame(Vintage = "1995", phen_stage = "2", wal_gdd_cab = NA)
wal2.96 <- data.frame(Vintage = "1996", phen_stage = "2", wal_gdd_cab = NA)
wal2.97 <- data.frame(Vintage = "1997", phen_stage = "2", wal_gdd_cab = NA)
wal2.98 <- data.frame(Vintage = "1998", phen_stage = "2", wal_gdd_cab = NA)
wal3.95 <- data.frame(Vintage = "1995", phen_stage = "3", wal_gdd_cab = NA)
wal3.96 <- data.frame(Vintage = "1996", phen_stage = "3", wal_gdd_cab = NA)
wal3.97 <- data.frame(Vintage = "1997", phen_stage = "3", wal_gdd_cab = NA)
wal3.98 <- data.frame(Vintage = "1998", phen_stage = "3", wal_gdd_cab = NA)

ag_wal_phen_cab = rbind(ag_wal_phen_cab, wal1.95, wal1.96, wal1.97, wal1.98, wal3.98, wal2.95, wal2.96,
                        wal2.97, wal2.98, wal3.95, wal3.96, wal3.97)
ag_wal_phen_cab <- ag_wal_phen_cab[order(ag_wal_phen_cab$Vintage), ]

#ag_wal_phen_syrah
wal1.95 <- data.frame(Vintage = "1995", phen_stage = "1", wal_gdd_syrah = NA)
wal1.96 <- data.frame(Vintage = "1996", phen_stage = "1", wal_gdd_syrah = NA)
wal1.97 <- data.frame(Vintage = "1997", phen_stage = "1", wal_gdd_syrah = NA)
wal1.98 <- data.frame(Vintage = "1998", phen_stage = "1", wal_gdd_syrah = NA)
wal2.95 <- data.frame(Vintage = "1995", phen_stage = "2", wal_gdd_syrah = NA)
wal2.96 <- data.frame(Vintage = "1996", phen_stage = "2", wal_gdd_syrah = NA)
wal2.97 <- data.frame(Vintage = "1997", phen_stage = "2", wal_gdd_syrah = NA)
wal2.98 <- data.frame(Vintage = "1998", phen_stage = "2", wal_gdd_syrah = NA)
wal3.95 <- data.frame(Vintage = "1995", phen_stage = "3", wal_gdd_syrah = NA)
wal3.96 <- data.frame(Vintage = "1996", phen_stage = "3", wal_gdd_syrah = NA)
wal3.97 <- data.frame(Vintage = "1997", phen_stage = "3", wal_gdd_syrah = NA)
wal3.98 <- data.frame(Vintage = "1998", phen_stage = "3", wal_gdd_syrah = NA)

ag_wal_phen_syrah = rbind(ag_wal_phen_syrah, wal1.95, wal1.96, wal1.97, wal1.98, wal3.98, wal2.95, wal2.96,
                        wal2.97, wal2.98, wal3.95, wal3.96, wal3.97)
ag_wal_phen_syrah <- ag_wal_phen_syrah[order(ag_wal_phen_syrah$Vintage), ]

#ag_wal_phen_mer
wal1.95 <- data.frame(Vintage = "1995", phen_stage = "1", wal_gdd_mer = NA)
wal1.96 <- data.frame(Vintage = "1996", phen_stage = "1", wal_gdd_mer = NA)
wal1.97 <- data.frame(Vintage = "1997", phen_stage = "1", wal_gdd_mer = NA)
wal1.98 <- data.frame(Vintage = "1998", phen_stage = "1", wal_gdd_mer = NA)
wal2.95 <- data.frame(Vintage = "1995", phen_stage = "2", wal_gdd_mer = NA)
wal2.96 <- data.frame(Vintage = "1996", phen_stage = "2", wal_gdd_mer = NA)
wal2.97 <- data.frame(Vintage = "1997", phen_stage = "2", wal_gdd_mer = NA)
wal2.98 <- data.frame(Vintage = "1998", phen_stage = "2", wal_gdd_mer = NA)
wal3.95 <- data.frame(Vintage = "1995", phen_stage = "3", wal_gdd_mer = NA)
wal3.96 <- data.frame(Vintage = "1996", phen_stage = "3", wal_gdd_mer = NA)
wal3.97 <- data.frame(Vintage = "1997", phen_stage = "3", wal_gdd_mer = NA)
wal3.98 <- data.frame(Vintage = "1998", phen_stage = "3", wal_gdd_mer = NA)

ag_wal_phen_mer = rbind(ag_wal_phen_mer, wal1.95, wal1.96, wal1.97, wal1.98, wal3.98, wal2.95, wal2.96,
                        wal2.97, wal2.98, wal3.95, wal3.96, wal3.97)
ag_wal_phen_mer <- ag_wal_phen_mer[order(ag_wal_phen_mer$Vintage), ]


#ag_dayt_phen_cab
dayt1.06 <- data.frame(Vintage = "2006", phen_stage = "1", dayt_gdd_cab = NA)
dayt2.06 <- data.frame(Vintage = "2006", phen_stage = "2", dayt_gdd_cab = NA)
dayt3.06 <- data.frame(Vintage = "2006", phen_stage = "3", dayt_gdd_cab = NA)
dayt3.05 <- data.frame(Vintage = "2005", phen_stage = "3", dayt_gdd_cab = NA)

ag_dayt_phen_cab = rbind(ag_dayt_phen_cab, dayt1.06, dayt2.06, dayt3.06, dayt3.05)
ag_dayt_phen_cab <- ag_dayt_phen_cab[order(ag_dayt_phen_cab$Vintage), ]

#ag_dayt_phen_syrah
dayt1.06 <- data.frame(Vintage = "2006", phen_stage = "1", dayt_gdd_syrah = NA)
dayt2.06 <- data.frame(Vintage = "2006", phen_stage = "2", dayt_gdd_syrah = NA)
dayt3.06 <- data.frame(Vintage = "2006", phen_stage = "3", dayt_gdd_syrah = NA)
dayt3.05 <- data.frame(Vintage = "2005", phen_stage = "3", dayt_gdd_syrah = NA)

ag_dayt_phen_syrah = rbind(ag_dayt_phen_syrah, dayt1.06, dayt2.06, dayt3.06, dayt3.05)
ag_dayt_phen_syrah <- ag_dayt_phen_syrah[order(ag_dayt_phen_syrah$Vintage), ]

#ag_dayt_phen_mer
dayt1.06 <- data.frame(Vintage = "2006", phen_stage = "1", dayt_gdd_mer = NA)
dayt2.06 <- data.frame(Vintage = "2006", phen_stage = "2", dayt_gdd_mer = NA)
dayt3.06 <- data.frame(Vintage = "2006", phen_stage = "3", dayt_gdd_mer = NA)
dayt3.05 <- data.frame(Vintage = "2005", phen_stage = "3", dayt_gdd_mer = NA)

ag_dayt_phen_mer = rbind(ag_dayt_phen_mer, dayt1.06, dayt2.06, dayt3.06, dayt3.05)
ag_dayt_phen_mer <- ag_dayt_phen_mer[order(ag_dayt_phen_mer$Vintage), ]

#ag_hat_phen_cab
hat3.13 <- data.frame(Vintage = "2013", phen_stage = "3", hat_gdd_cab = NA)
hat3.14 <- data.frame(Vintage = "2014", phen_stage = "3", hat_gdd_cab = NA)
hat2.13 <- data.frame(Vintage = "2013", phen_stage = "2", hat_gdd_cab = NA)
hat1.13 <- data.frame(Vintage = "2013", phen_stage = "1", hat_gdd_cab = NA)

ag_hat_phen_cab = rbind(ag_hat_phen_cab, hat3.13, hat2.13, hat1.13, hat3.14)
ag_hat_phen_cab <- ag_hat_phen_cab[order(ag_dayt_phen_mer$Vintage), ]

#ag_hat_phen_syrah
hat3.13 <- data.frame(Vintage = "2013", phen_stage = "3", hat_gdd_syrah = NA)
hat3.14 <- data.frame(Vintage = "2014", phen_stage = "3", hat_gdd_syrah = NA)
hat2.13 <- data.frame(Vintage = "2013", phen_stage = "2", hat_gdd_syrah = NA)
hat1.13 <- data.frame(Vintage = "2013", phen_stage = "1", hat_gdd_syrah = NA)

ag_hat_phen_syrah = rbind(ag_hat_phen_syrah, hat3.13, hat2.13, hat1.13, hat3.14)
ag_hat_phen_syrah <- ag_hat_phen_syrah[order(ag_dayt_phen_mer$Vintage), ]

#ag_hat_phen_mer
hat3.13 <- data.frame(Vintage = "2013", phen_stage = "3", hat_gdd_mer = NA)
hat3.14 <- data.frame(Vintage = "2014", phen_stage = "3", hat_gdd_mer = NA)
hat2.13 <- data.frame(Vintage = "2013", phen_stage = "2", hat_gdd_mer = NA)
hat1.13 <- data.frame(Vintage = "2013", phen_stage = "1", hat_gdd_mer = NA)

ag_hat_phen_mer = rbind(ag_hat_phen_mer, hat3.13, hat2.13, hat1.13, hat3.14)
ag_hat_phen_mer <- ag_hat_phen_mer[order(ag_dayt_phen_mer$Vintage), ]

gdd_agg_phen <- merge(ag_hat_phen_cab, ag_dayt_phen_cab)
gdd_agg_phen <- merge(gdd_agg_phen, ag_wal_phen_cab)
gdd_agg_phen <- merge(gdd_agg_phen, ag_eph_phen_cab)
gdd_agg_phen <- merge(gdd_agg_phen, ag_dalle_phen_cab)
gdd_agg_phen <- merge(gdd_agg_phen, ag_dayt_phen_syrah)
gdd_agg_phen <- merge(gdd_agg_phen, ag_hat_phen_syrah)
gdd_agg_phen <- merge(gdd_agg_phen, ag_wal_phen_syrah)
gdd_agg_phen <- merge(gdd_agg_phen, ag_eph_phen_syrah)
gdd_agg_phen <- merge(gdd_agg_phen, ag_dalle_phen_syrah)
gdd_agg_phen <- merge(gdd_agg_phen, ag_dayt_phen_mer)
gdd_agg_phen <- merge(gdd_agg_phen, ag_hat_phen_mer)
gdd_agg_phen <- merge(gdd_agg_phen, ag_wal_phen_mer)
gdd_agg_phen <- merge(gdd_agg_phen, ag_eph_phen_mer)
gdd_agg_phen <- merge(gdd_agg_phen, ag_dalle_phen_mer)

#averaging by location for each variety
gdd_agg_phen$gdd_avg_phen_cab <- rowMeans(gdd_agg_phen[, 3:7], na.rm = TRUE)
gdd_agg_phen$gdd_avg_phen_syrah <- rowMeans(gdd_agg_phen[, 8:12], na.rm = TRUE)
gdd_agg_phen$gdd_avg_phen_mer <- rowMeans(gdd_agg_phen[, 13:17], na.rm = TRUE)

#Year

#cabernet
ag_hat_year_cab <- aggregate(hat$gddbase_cab, by = list(hat$year), FUN=sum) 
colnames(ag_hat_year_cab)[1] <- c("Vintage")
colnames(ag_hat_year_cab)[2] <- c("hat_gdd_cab")

ag_dayt_year_cab <- aggregate(dayt$gddbase_cab, by = list(dayt$year), FUN=sum) 
colnames(ag_dayt_year_cab)[1] <- c("Vintage")
colnames(ag_dayt_year_cab)[2] <- c("dayt_gdd_cab")

ag_wal_year_cab <- aggregate(wal$gddbase_cab, by = list(wal$year), FUN=sum) 
colnames(ag_wal_year_cab)[1] <- c("Vintage")
colnames(ag_wal_year_cab)[2] <- c("wal_gdd_cab")

ag_eph_year_cab <- aggregate(eph$gddbase_cab, by = list(eph$year), FUN=sum) 
colnames(ag_eph_year_cab)[1] <- c("Vintage")
colnames(ag_eph_year_cab)[2] <- c("eph_gdd_cab")

ag_dalle_year_cab <- aggregate(dalle$gddbase_cab, by = list(dalle$year), FUN=sum) 
colnames(ag_dalle_year_cab)[1] <- c("Vintage")
colnames(ag_dalle_year_cab)[2] <- c("dalle_gdd_cab")

#syrah
ag_hat_year_syrah <- aggregate(hat$gddbase_syrah, by = list(hat$year), FUN=sum) 
colnames(ag_hat_year_syrah)[1] <- c("Vintage")
colnames(ag_hat_year_syrah)[2] <- c("hat_gdd_syrah")

ag_dayt_year_syrah <- aggregate(dayt$gddbase_syrah, by = list(dayt$year), FUN=sum) 
colnames(ag_dayt_year_syrah)[1] <- c("Vintage")
colnames(ag_dayt_year_syrah)[2] <- c("dayt_gdd_syrah")

ag_wal_year_syrah <- aggregate(wal$gddbase_syrah, by = list(wal$year), FUN=sum) 
colnames(ag_wal_year_syrah)[1] <- c("Vintage")
colnames(ag_wal_year_syrah)[2] <- c("wal_gdd_syrah")

ag_eph_year_syrah <- aggregate(eph$gddbase_syrah, by = list(eph$year), FUN=sum) 
colnames(ag_eph_year_syrah)[1] <- c("Vintage")
colnames(ag_eph_year_syrah)[2] <- c("eph_gdd_syrah")

ag_dalle_year_syrah <- aggregate(dalle$gddbase_syrah, by = list(dalle$year), FUN=sum) 
colnames(ag_dalle_year_syrah)[1] <- c("Vintage")
colnames(ag_dalle_year_syrah)[2] <- c("dalle_gdd_syrah")

#merlot
ag_hat_year_mer <- aggregate(hat$gddbase_mer, by = list(hat$year), FUN=sum) 
colnames(ag_hat_year_mer)[1] <- c("Vintage")
colnames(ag_hat_year_mer)[2] <- c("hat_gdd_mer")

ag_dayt_year_mer <- aggregate(dayt$gddbase_mer, by = list(dayt$year), FUN=sum) 
colnames(ag_dayt_year_mer)[1] <- c("Vintage")
colnames(ag_dayt_year_mer)[2] <- c("dayt_gdd_mer")

ag_wal_year_mer <- aggregate(wal$gddbase_mer, by = list(wal$year), FUN=sum) 
colnames(ag_wal_year_mer)[1] <- c("Vintage")
colnames(ag_wal_year_mer)[2] <- c("wal_gdd_mer")

ag_eph_year_mer <- aggregate(eph$gddbase_mer, by = list(eph$year), FUN=sum) 
colnames(ag_eph_year_mer)[1] <- c("Vintage")
colnames(ag_eph_year_mer)[2] <- c("eph_gdd_mer")

ag_dalle_year_mer <- aggregate(dalle$gddbase_mer, by = list(dalle$year), FUN=sum) 
colnames(ag_dalle_year_mer)[1] <- c("Vintage")
colnames(ag_dalle_year_mer)[2] <- c("dalle_gdd_mer")

#editing stations with missing years
#ag_dalle_year_cab
dalle1.17 <- data.frame(Vintage = "2017", dalle_gdd_cab = NA)
dalle1.18 <- data.frame(Vintage = "2018", dalle_gdd_cab = NA)

ag_dalle_year_cab = rbind(ag_dalle_year_cab, dalle1.17, dalle1.18)
ag_dalle_year_cab <- ag_dalle_year_cab[order(ag_dalle_year_cab$Vintage), ]

#ag_dalle_year_syrah
dalle1.17 <- data.frame(Vintage = "2017", dalle_gdd_syrah = NA)
dalle1.18 <- data.frame(Vintage = "2018", dalle_gdd_syrah = NA)

ag_dalle_year_syrah = rbind(ag_dalle_year_syrah, dalle1.17, dalle1.18)
ag_dalle_year_syrah <- ag_dalle_year_syrah[order(ag_dalle_year_syrah$Vintage), ]

#ag_dalle_year_mer
dalle1.17 <- data.frame(Vintage = "2017", dalle_gdd_mer = NA)
dalle1.18 <- data.frame(Vintage = "2018", dalle_gdd_mer = NA)

ag_dalle_year_mer = rbind(ag_dalle_year_mer, dalle1.17, dalle1.18)
ag_dalle_year_mer <- ag_dalle_year_mer[order(ag_dalle_year_mer$Vintage), ]

#ag_eph_year_cab
ag_eph_year_cab$eph_gdd_cab[which(ag_eph_year_cab$Vintage=="2016")] <- NA 
eph1.17 <- data.frame(Vintage = "2017", eph_gdd_cab = NA)
eph1.18 <- data.frame(Vintage = "2018", eph_gdd_cab = NA)

ag_eph_year_cab = rbind(ag_eph_year_cab, eph1.17, eph1.18)
ag_eph_year_cab <- ag_eph_year_cab[order(ag_eph_year_cab$Vintage), ]

#ag_eph_year_syrah
ag_eph_year_syrah$eph_gdd_syrah[which(ag_eph_year_syrah$Vintage=="2016")] <- NA 
eph1.17 <- data.frame(Vintage = "2017", eph_gdd_syrah = NA)
eph1.18 <- data.frame(Vintage = "2018", eph_gdd_syrah = NA)

ag_eph_year_syrah = rbind(ag_eph_year_syrah, eph1.17, eph1.18)
ag_eph_year_syrah <- ag_eph_year_syrah[order(ag_eph_year_syrah$Vintage), ]

#ag_eph_year_mer
ag_eph_year_mer$eph_gdd_mer[which(ag_eph_year_mer$Vintage=="2016")] <- NA 
eph1.17 <- data.frame(Vintage = "2017", eph_gdd_mer = NA)
eph1.18 <- data.frame(Vintage = "2018", eph_gdd_mer = NA)

ag_eph_year_mer = rbind(ag_eph_year_mer, eph1.17, eph1.18)
ag_eph_year_mer <- ag_eph_year_mer[order(ag_eph_year_mer$Vintage), ]

#ag_wal_year_cab
wal1.95 <- data.frame(Vintage = "1995", wal_gdd_cab = NA)
wal1.96 <- data.frame(Vintage = "1996", wal_gdd_cab = NA)
wal1.97 <- data.frame(Vintage = "1997", wal_gdd_cab = NA)
wal1.98 <- data.frame(Vintage = "1998", wal_gdd_cab = NA)

ag_wal_year_cab = rbind(ag_wal_year_cab, wal1.95, wal1.96, wal1.97, wal1.98)
ag_wal_year_cab <- ag_wal_year_cab[order(ag_wal_year_cab$Vintage), ]

#ag_wal_year_syrah
wal1.95 <- data.frame(Vintage = "1995", wal_gdd_syrah = NA)
wal1.96 <- data.frame(Vintage = "1996", wal_gdd_syrah = NA)
wal1.97 <- data.frame(Vintage = "1997", wal_gdd_syrah = NA)
wal1.98 <- data.frame(Vintage = "1998", wal_gdd_syrah = NA)

ag_wal_year_syrah = rbind(ag_wal_year_syrah, wal1.95, wal1.96, wal1.97, wal1.98)
ag_wal_year_syrah <- ag_wal_year_syrah[order(ag_wal_year_syrah$Vintage), ]

#ag_wal_year_mer
wal1.95 <- data.frame(Vintage = "1995", wal_gdd_mer = NA)
wal1.96 <- data.frame(Vintage = "1996", wal_gdd_mer = NA)
wal1.97 <- data.frame(Vintage = "1997", wal_gdd_mer = NA)
wal1.98 <- data.frame(Vintage = "1998", wal_gdd_mer = NA)

ag_wal_year_mer = rbind(ag_wal_year_mer, wal1.95, wal1.96, wal1.97, wal1.98)
ag_wal_year_mer <- ag_wal_year_mer[order(ag_wal_year_mer$Vintage), ]

#ag_dayt_year_cab
dayt1.06 <- data.frame(Vintage = "2006", dayt_gdd_cab = NA)

ag_dayt_year_cab = rbind(ag_dayt_year_cab, dayt1.06)
ag_dayt_year_cab <- ag_dayt_year_cab[order(ag_dayt_year_cab$Vintage), ]

#ag_dayt_year_syrah
dayt1.06 <- data.frame(Vintage = "2006", dayt_gdd_syrah = NA)

ag_dayt_year_syrah = rbind(ag_dayt_year_syrah, dayt1.06)
ag_dayt_year_syrah <- ag_dayt_year_syrah[order(ag_dayt_year_syrah$Vintage), ]

#ag_dayt_phen_mer
dayt1.06 <- data.frame(Vintage = "2006", dayt_gdd_mer = NA)

ag_dayt_year_mer = rbind(ag_dayt_year_mer, dayt1.06)
ag_dayt_year_mer <- ag_dayt_year_mer[order(ag_dayt_year_mer$Vintage), ]

#ag_hat_phen_cab
hat1.13 <- data.frame(Vintage = "2013", hat_gdd_cab = NA)

ag_hat_year_cab = rbind(ag_hat_year_cab, hat1.13)
ag_hat_year_cab <- ag_hat_year_cab[order(ag_dayt_year_mer$Vintage), ]

#ag_hat_phen_syrah
hat1.13 <- data.frame(Vintage = "2013", hat_gdd_syrah = NA)

ag_hat_year_syrah = rbind(ag_hat_year_syrah, hat1.13)
ag_hat_year_syrah <- ag_hat_year_syrah[order(ag_dayt_year_mer$Vintage), ]

#ag_hat_phen_mer
hat1.13 <- data.frame(Vintage = "2013", hat_gdd_mer = NA)

ag_hat_year_mer = rbind(ag_hat_year_mer, hat1.13)
ag_hat_year_mer <- ag_hat_year_mer[order(ag_dayt_year_mer$Vintage), ]


gdd_agg_year <- merge(ag_hat_year_cab, ag_dayt_year_cab)
gdd_agg_year <- merge(gdd_agg_year, ag_wal_year_cab)
gdd_agg_year <- merge(gdd_agg_year, ag_eph_year_cab)
gdd_agg_year <- merge(gdd_agg_year, ag_dalle_year_cab)
gdd_agg_year <- merge(gdd_agg_year, ag_hat_year_syrah)
gdd_agg_year <- merge(gdd_agg_year, ag_dayt_year_syrah)
gdd_agg_year <- merge(gdd_agg_year, ag_wal_year_syrah)
gdd_agg_year <- merge(gdd_agg_year, ag_eph_year_syrah)
gdd_agg_year <- merge(gdd_agg_year, ag_dalle_year_syrah)
gdd_agg_year <- merge(gdd_agg_year, ag_hat_year_mer)
gdd_agg_year <- merge(gdd_agg_year, ag_dayt_year_mer)
gdd_agg_year <- merge(gdd_agg_year, ag_wal_year_mer)
gdd_agg_year <- merge(gdd_agg_year, ag_eph_year_mer)
gdd_agg_year <- merge(gdd_agg_year, ag_dalle_year_mer)

#averaging by location for each variety
gdd_agg_year$gdd_avg_year_cab <- rowMeans(gdd_agg_year[, 2:6], na.rm = TRUE)
gdd_agg_year$gdd_avg_year_syrah <- rowMeans(gdd_agg_year[, 7:11], na.rm = TRUE)
gdd_agg_year$gdd_avg_year_mer <- rowMeans(gdd_agg_year[, 12:16], na.rm = TRUE)

#Calculating Precipitation at each location by year and phen stage

#Base GDD values - need individual per variety
#syrah
hat$prcpbase_syrah <- ifelse(is.na(hat$phen_stage_syrah), 0, hat$PRCP)
dayt$prcpbase_syrah <- ifelse(is.na(dayt$phen_stage_syrah), 0, dayt$PRCP)
wal$prcpbase_syrah <- ifelse(is.na(wal$phen_stage_syrah), 0, wal$PRCP)
eph$prcpbase_syrah <- ifelse(is.na(eph$phen_stage_syrah), 0, eph$PRCP)
dalle$prcpbase_syrah <- ifelse(is.na(dalle$phen_stage_syrah), 0, dalle$PRCP)
#cabernet
hat$prcpbase_cab <- ifelse(is.na(hat$phen_stage_cab), 0, hat$PRCP)
dayt$prcpbase_cab <- ifelse(is.na(dayt$phen_stage_cab), 0, dayt$PRCP)
wal$prcpbase_cab <- ifelse(is.na(wal$phen_stage_cab), 0, wal$PRCP)
eph$prcpbase_cab <- ifelse(is.na(eph$phen_stage_cab), 0, eph$PRCP)
dalle$prcpbase_cab <- ifelse(is.na(dalle$phen_stage_cab), 0, dalle$PRCP)
#merlot
hat$prcpbase_mer <- ifelse(is.na(hat$phen_stage_mer), 0, hat$PRCP)
dayt$prcpbase_mer <- ifelse(is.na(dayt$phen_stage_mer), 0, dayt$PRCP)
wal$prcpbase_mer <- ifelse(is.na(wal$phen_stage_mer), 0, wal$PRCP)
eph$prcpbase_mer <- ifelse(is.na(eph$phen_stage_mer), 0, eph$PRCP)
dalle$prcpbase_mer <- ifelse(is.na(dalle$phen_stage_mer), 0, dalle$PRCP)

#Summation by phenological stage and variety
#merlot
hat$prcpsum_phen_mer <- ave(hat$prcpbase_mer, by = list(hat$year, hat$phen_stage_mer), FUN = cumsum)
dayt$prcpsum_phen_mer <- ave(dayt$prcpbase_mer, by = list(dayt$year, dayt$phen_stage_mer), FUN = cumsum)
wal$prcpsum_phen_mer <- ave(wal$prcpbase_mer, by = list(wal$year, wal$phen_stage_mer), FUN = cumsum)
eph$prcpsum_phen_mer <- ave(eph$prcpbase_mer, by = list(eph$year, eph$phen_stage_mer), FUN = cumsum)
dalle$prcpsum_phen_mer <- ave(dalle$prcpbase_mer, by = list(dalle$year, dalle$phen_stage_mer), FUN = cumsum)
#syrah
hat$prcpsum_phen_syrah <- ave(hat$prcpbase_syrah, by = list(hat$year, hat$phen_stage_syrah), FUN = cumsum)
dayt$prcpsum_phen_syrah <- ave(dayt$prcpbase_syrah, by = list(dayt$year, dayt$phen_stage_syrah), FUN = cumsum)
wal$prcpsum_phen_syrah <- ave(wal$prcpbase_syrah, by = list(wal$year, wal$phen_stage_syrah), FUN = cumsum)
eph$prcpsum_phen_syrah <- ave(eph$prcpbase_syrah, by = list(eph$year, eph$phen_stage_syrah), FUN = cumsum)
dalle$prcpsum_phen_syrah <- ave(dalle$prcpbase_syrah, by = list(dalle$year, dalle$phen_stage_syrah), FUN = cumsum)
#cabernet
hat$prcpsum_phen_cab <- ave(hat$prcpbase_cab, by = list(hat$year, hat$phen_stage_cab), FUN = cumsum)
dayt$prcpsum_phen_cab <- ave(dayt$prcpbase_cab, by = list(dayt$year, dayt$phen_stage_cab), FUN = cumsum)
wal$prcpsum_phen_cab <- ave(wal$prcpbase_cab, by = list(wal$year, wal$phen_stage_cab), FUN = cumsum)
eph$prcpsum_phen_cab <- ave(eph$prcpbase_cab, by = list(eph$year, eph$phen_stage_cab), FUN = cumsum)
dalle$prcpsum_phen_cab <- ave(dalle$prcpbase_cab, by = list(dalle$year, dalle$phen_stage_cab), FUN = cumsum)

#Summation by year and variety
#merlot
hat$prcpsum_year_mer <- ave(hat$prcpbase_mer, hat$year, FUN = cumsum)
dayt$prcpsum_year_mer <- ave(dayt$prcpbase_mer, dayt$year, FUN = cumsum)
wal$prcpsum_year_mer <- ave(wal$prcpbase_mer, wal$year, FUN = cumsum)
eph$prcpsum_year_mer <- ave(eph$prcpbase_mer, eph$year, FUN = cumsum)
dalle$prcpsum_year_mer <- ave(dalle$prcpbase_mer, dalle$year, FUN = cumsum)
#syrah
hat$prcpsum_year_syrah <- ave(hat$prcpbase_syrah, hat$year, FUN = cumsum)
dayt$prcpsum_year_syrah <- ave(dayt$prcpbase_syrah, dayt$year, FUN = cumsum)
wal$prcpsum_year_syrah <- ave(wal$prcpbase_syrah, wal$year, FUN = cumsum)
eph$prcpsum_year_syrah <- ave(eph$prcpbase_syrah, eph$year, FUN = cumsum)
dalle$prcpsum_year_syrah <- ave(dalle$prcpbase_syrah, dalle$year, FUN = cumsum)
#cabernet
hat$prcpsum_year_cab <- ave(hat$prcpbase_cab, hat$year, FUN = cumsum)
dayt$prcpsum_year_cab <- ave(dayt$prcpbase_cab, dayt$year, FUN = cumsum)
wal$prcpsum_year_cab <- ave(wal$prcpbase_cab, wal$year, FUN = cumsum)
eph$prcpsum_year_cab <- ave(eph$prcpbase_cab, eph$year, FUN = cumsum)
dalle$prcpsum_year_cab <- ave(dalle$prcpbase_cab, dalle$year, FUN = cumsum)

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

agp_wal_phen_cab <- aggregate(wal$prcpbase_cab, by = list(wal$year, wal$phen_stage_cab), FUN=sum) 
colnames(agp_wal_phen_cab)[1] <- c("Vintage")
colnames(agp_wal_phen_cab)[2] <- c("phen_stage")
colnames(agp_wal_phen_cab)[3] <- c("wal_prcp_cab")

agp_eph_phen_cab <- aggregate(eph$prcpbase_cab, by = list(eph$year, eph$phen_stage_cab), FUN=sum) 
colnames(agp_eph_phen_cab)[1] <- c("Vintage")
colnames(agp_eph_phen_cab)[2] <- c("phen_stage")
colnames(agp_eph_phen_cab)[3] <- c("eph_prcp_cab")

agp_dalle_phen_cab <- aggregate(dalle$prcpbase_cab, by = list(dalle$year, dalle$phen_stage_cab), FUN=sum) 
colnames(agp_dalle_phen_cab)[1] <- c("Vintage")
colnames(agp_dalle_phen_cab)[2] <- c("phen_stage")
colnames(agp_dalle_phen_cab)[3] <- c("dalle_prcp_cab")

#syrah
agp_hat_phen_syrah <- aggregate(hat$prcpbase_syrah, by = list(hat$year, hat$phen_stage_syrah), FUN=sum) 
colnames(agp_hat_phen_syrah)[1] <- c("Vintage")
colnames(agp_hat_phen_syrah)[2] <- c("phen_stage")
colnames(agp_hat_phen_syrah)[3] <- c("hat_prcp_syrah")

agp_dayt_phen_syrah <- aggregate(dayt$prcpbase_syrah, by = list(dayt$year, dayt$phen_stage_syrah), FUN=sum) 
colnames(agp_dayt_phen_syrah)[1] <- c("Vintage")
colnames(agp_dayt_phen_syrah)[2] <- c("phen_stage")
colnames(agp_dayt_phen_syrah)[3] <- c("dayt_prcp_syrah")

agp_wal_phen_syrah <- aggregate(wal$prcpbase_syrah, by = list(wal$year, wal$phen_stage_syrah), FUN=sum) 
colnames(agp_wal_phen_syrah)[1] <- c("Vintage")
colnames(agp_wal_phen_syrah)[2] <- c("phen_stage")
colnames(agp_wal_phen_syrah)[3] <- c("wal_prcp_syrah")

agp_eph_phen_syrah <- aggregate(eph$prcpbase_syrah, by = list(eph$year, eph$phen_stage_syrah), FUN=sum) 
colnames(agp_eph_phen_syrah)[1] <- c("Vintage")
colnames(agp_eph_phen_syrah)[2] <- c("phen_stage")
colnames(agp_eph_phen_syrah)[3] <- c("eph_prcp_syrah")

agp_dalle_phen_syrah <- aggregate(dalle$prcpbase_syrah, by = list(dalle$year, dalle$phen_stage_syrah), FUN=sum) 
colnames(agp_dalle_phen_syrah)[1] <- c("Vintage")
colnames(agp_dalle_phen_syrah)[2] <- c("phen_stage")
colnames(agp_dalle_phen_syrah)[3] <- c("dalle_prcp_syrah")

#merlot
agp_hat_phen_mer <- aggregate(hat$prcpbase_mer, by = list(hat$year, hat$phen_stage_mer), FUN=sum) 
colnames(agp_hat_phen_mer)[1] <- c("Vintage")
colnames(agp_hat_phen_mer)[2] <- c("phen_stage")
colnames(agp_hat_phen_mer)[3] <- c("hat_prcp_mer")

agp_dayt_phen_mer <- aggregate(dayt$prcpbase_mer, by = list(dayt$year, dayt$phen_stage_mer), FUN=sum) 
colnames(agp_dayt_phen_mer)[1] <- c("Vintage")
colnames(agp_dayt_phen_mer)[2] <- c("phen_stage")
colnames(agp_dayt_phen_mer)[3] <- c("dayt_prcp_mer")

agp_wal_phen_mer <- aggregate(wal$prcpbase_mer, by = list(wal$year, wal$phen_stage_mer), FUN=sum) 
colnames(agp_wal_phen_mer)[1] <- c("Vintage")
colnames(agp_wal_phen_mer)[2] <- c("phen_stage")
colnames(agp_wal_phen_mer)[3] <- c("wal_prcp_mer")

agp_eph_phen_mer <- aggregate(eph$prcpbase_mer, by = list(eph$year, eph$phen_stage_mer), FUN=sum) 
colnames(agp_eph_phen_mer)[1] <- c("Vintage")
colnames(agp_eph_phen_mer)[2] <- c("phen_stage")
colnames(agp_eph_phen_mer)[3] <- c("eph_prcp_mer")

agp_dalle_phen_mer <- aggregate(dalle$prcpbase_mer, by = list(dalle$year, dalle$phen_stage_mer), FUN=sum) 
colnames(agp_dalle_phen_mer)[1] <- c("Vintage")
colnames(agp_dalle_phen_mer)[2] <- c("phen_stage")
colnames(agp_dalle_phen_mer)[3] <- c("dalle_prcp_mer")

###
#editing stations with missing years
#agp_dalle_phen_cab
dalle2.16 <- data.frame(Vintage = "2016", phen_stage = "2", dalle_prcp_cab = NA)
dalle3.16 <- data.frame(Vintage = "2016", phen_stage = "3", dalle_prcp_cab = NA)
dalle1.17 <- data.frame(Vintage = "2017", phen_stage = "1", dalle_prcp_cab = NA)
dalle2.17 <- data.frame(Vintage = "2017", phen_stage = "2", dalle_prcp_cab = NA)
dalle3.17 <- data.frame(Vintage = "2017", phen_stage = "3", dalle_prcp_cab = NA)
dalle1.18 <- data.frame(Vintage = "2018", phen_stage = "1", dalle_prcp_cab = NA)
dalle2.18 <- data.frame(Vintage = "2018", phen_stage = "2", dalle_prcp_cab = NA)
dalle3.18 <- data.frame(Vintage = "2018", phen_stage = "3", dalle_prcp_cab = NA)

agp_dalle_phen_cab = rbind(agp_dalle_phen_cab, dalle2.16, dalle3.16, dalle1.17, dalle2.17, dalle3.17, dalle1.18, dalle2.18, dalle3.18)
agp_dalle_phen_cab <- agp_dalle_phen_cab[order(agp_dalle_phen_cab$Vintage), ]

#agp_dalle_phen_syrah
dalle2.16 <- data.frame(Vintage = "2016", phen_stage = "2", dalle_prcp_syrah = NA)
dalle3.16 <- data.frame(Vintage = "2016", phen_stage = "3", dalle_prcp_syrah = NA)
dalle1.17 <- data.frame(Vintage = "2017", phen_stage = "1", dalle_prcp_syrah = NA)
dalle2.17 <- data.frame(Vintage = "2017", phen_stage = "2", dalle_prcp_syrah = NA)
dalle3.17 <- data.frame(Vintage = "2017", phen_stage = "3", dalle_prcp_syrah = NA)
dalle1.18 <- data.frame(Vintage = "2018", phen_stage = "1", dalle_prcp_syrah = NA)
dalle2.18 <- data.frame(Vintage = "2018", phen_stage = "2", dalle_prcp_syrah = NA)
dalle3.18 <- data.frame(Vintage = "2018", phen_stage = "3", dalle_prcp_syrah = NA)

agp_dalle_phen_syrah = rbind(agp_dalle_phen_syrah, dalle2.16, dalle3.16, dalle1.17, dalle2.17, dalle3.17, dalle1.18, dalle2.18, dalle3.18)
agp_dalle_phen_syrah <- agp_dalle_phen_syrah[order(agp_dalle_phen_syrah$Vintage), ]

#agp_dalle_phen_mer
dalle2.16 <- data.frame(Vintage = "2016", phen_stage = "2", dalle_prcp_mer = NA)
dalle3.16 <- data.frame(Vintage = "2016", phen_stage = "3", dalle_prcp_mer = NA)
dalle1.17 <- data.frame(Vintage = "2017", phen_stage = "1", dalle_prcp_mer = NA)
dalle2.17 <- data.frame(Vintage = "2017", phen_stage = "2", dalle_prcp_mer = NA)
dalle3.17 <- data.frame(Vintage = "2017", phen_stage = "3", dalle_prcp_mer = NA)
dalle1.18 <- data.frame(Vintage = "2018", phen_stage = "1", dalle_prcp_mer = NA)
dalle2.18 <- data.frame(Vintage = "2018", phen_stage = "2", dalle_prcp_mer = NA)
dalle3.18 <- data.frame(Vintage = "2018", phen_stage = "3", dalle_prcp_mer = NA)

agp_dalle_phen_mer = rbind(agp_dalle_phen_mer, dalle2.16, dalle3.16, dalle1.17, dalle2.17, dalle3.17, dalle1.18, dalle2.18, dalle3.18)
agp_dalle_phen_mer <- agp_dalle_phen_mer[order(agp_dalle_phen_mer$Vintage), ]

#agp_eph_phen_cab
eph1.16 <- data.frame(Vintage = "2016", phen_stage = "1", eph_prcp_cab = NA)
eph2.16 <- data.frame(Vintage = "2016", phen_stage = "2", eph_prcp_cab = NA)
eph3.16 <- data.frame(Vintage = "2016", phen_stage = "3", eph_prcp_cab = NA)
eph1.17 <- data.frame(Vintage = "2017", phen_stage = "1", eph_prcp_cab = NA)
eph2.17 <- data.frame(Vintage = "2017", phen_stage = "2", eph_prcp_cab = NA)
eph3.17 <- data.frame(Vintage = "2017", phen_stage = "3", eph_prcp_cab = NA)
eph1.18 <- data.frame(Vintage = "2018", phen_stage = "1", eph_prcp_cab = NA)
eph2.18 <- data.frame(Vintage = "2018", phen_stage = "2", eph_prcp_cab = NA)
eph3.18 <- data.frame(Vintage = "2018", phen_stage = "3", eph_prcp_cab = NA)

agp_eph_phen_cab = rbind(agp_eph_phen_cab, eph1.16, eph2.16, eph3.16, eph1.17, eph2.17, eph3.17, eph1.18, eph2.18, eph3.18)
agp_eph_phen_cab <- agp_eph_phen_cab[order(agp_eph_phen_cab$Vintage), ]

#agp_eph_phen_syrah
eph1.16 <- data.frame(Vintage = "2016", phen_stage = "1", eph_prcp_syrah = NA)
eph2.16 <- data.frame(Vintage = "2016", phen_stage = "2", eph_prcp_syrah = NA)
eph3.16 <- data.frame(Vintage = "2016", phen_stage = "3", eph_prcp_syrah = NA)
eph1.17 <- data.frame(Vintage = "2017", phen_stage = "1", eph_prcp_syrah = NA)
eph2.17 <- data.frame(Vintage = "2017", phen_stage = "2", eph_prcp_syrah = NA)
eph3.17 <- data.frame(Vintage = "2017", phen_stage = "3", eph_prcp_syrah = NA)
eph1.18 <- data.frame(Vintage = "2018", phen_stage = "1", eph_prcp_syrah = NA)
eph2.18 <- data.frame(Vintage = "2018", phen_stage = "2", eph_prcp_syrah = NA)
eph3.18 <- data.frame(Vintage = "2018", phen_stage = "3", eph_prcp_syrah = NA)

agp_eph_phen_syrah = rbind(agp_eph_phen_syrah, eph1.16, eph2.16, eph3.16, eph1.17, eph2.17, eph3.17, eph1.18, eph2.18, eph3.18)
agp_eph_phen_syrah <- agp_eph_phen_syrah[order(agp_eph_phen_syrah$Vintage), ]

#agp_eph_phen_mer
eph2.16 <- data.frame(Vintage = "2016", phen_stage = "2", eph_prcp_mer = NA)
eph3.16 <- data.frame(Vintage = "2016", phen_stage = "3", eph_prcp_mer = NA)
eph1.17 <- data.frame(Vintage = "2017", phen_stage = "1", eph_prcp_mer = NA)
eph2.17 <- data.frame(Vintage = "2017", phen_stage = "2", eph_prcp_mer = NA)
eph3.17 <- data.frame(Vintage = "2017", phen_stage = "3", eph_prcp_mer = NA)
eph1.18 <- data.frame(Vintage = "2018", phen_stage = "1", eph_prcp_mer = NA)
eph2.18 <- data.frame(Vintage = "2018", phen_stage = "2", eph_prcp_mer = NA)
eph3.18 <- data.frame(Vintage = "2018", phen_stage = "3", eph_prcp_mer = NA)

agp_eph_phen_mer = rbind(agp_eph_phen_mer, eph2.16, eph3.16, eph1.17, eph2.17, eph3.17, eph1.18, eph2.18, eph3.18)
agp_eph_phen_mer <- agp_eph_phen_mer[order(agp_eph_phen_mer$Vintage), ]

#agp_wal_phen_cab
wal1.95 <- data.frame(Vintage = "1995", phen_stage = "1", wal_prcp_cab = NA)
wal1.96 <- data.frame(Vintage = "1996", phen_stage = "1", wal_prcp_cab = NA)
wal1.97 <- data.frame(Vintage = "1997", phen_stage = "1", wal_prcp_cab = NA)
wal1.98 <- data.frame(Vintage = "1998", phen_stage = "1", wal_prcp_cab = NA)
wal2.95 <- data.frame(Vintage = "1995", phen_stage = "2", wal_prcp_cab = NA)
wal2.96 <- data.frame(Vintage = "1996", phen_stage = "2", wal_prcp_cab = NA)
wal2.97 <- data.frame(Vintage = "1997", phen_stage = "2", wal_prcp_cab = NA)
wal2.98 <- data.frame(Vintage = "1998", phen_stage = "2", wal_prcp_cab = NA)
wal3.95 <- data.frame(Vintage = "1995", phen_stage = "3", wal_prcp_cab = NA)
wal3.96 <- data.frame(Vintage = "1996", phen_stage = "3", wal_prcp_cab = NA)
wal3.97 <- data.frame(Vintage = "1997", phen_stage = "3", wal_prcp_cab = NA)
wal3.98 <- data.frame(Vintage = "1998", phen_stage = "3", wal_prcp_cab = NA)

agp_wal_phen_cab = rbind(agp_wal_phen_cab, wal1.95, wal1.96, wal1.97, wal1.98, wal3.98, wal2.95, wal2.96,
                        wal2.97, wal2.98, wal3.95, wal3.96, wal3.97)
agp_wal_phen_cab <- agp_wal_phen_cab[order(agp_wal_phen_cab$Vintage), ]

#agp_wal_phen_syrah
wal1.95 <- data.frame(Vintage = "1995", phen_stage = "1", wal_prcp_syrah = NA)
wal1.96 <- data.frame(Vintage = "1996", phen_stage = "1", wal_prcp_syrah = NA)
wal1.97 <- data.frame(Vintage = "1997", phen_stage = "1", wal_prcp_syrah = NA)
wal1.98 <- data.frame(Vintage = "1998", phen_stage = "1", wal_prcp_syrah = NA)
wal2.95 <- data.frame(Vintage = "1995", phen_stage = "2", wal_prcp_syrah = NA)
wal2.96 <- data.frame(Vintage = "1996", phen_stage = "2", wal_prcp_syrah = NA)
wal2.97 <- data.frame(Vintage = "1997", phen_stage = "2", wal_prcp_syrah = NA)
wal2.98 <- data.frame(Vintage = "1998", phen_stage = "2", wal_prcp_syrah = NA)
wal3.95 <- data.frame(Vintage = "1995", phen_stage = "3", wal_prcp_syrah = NA)
wal3.96 <- data.frame(Vintage = "1996", phen_stage = "3", wal_prcp_syrah = NA)
wal3.97 <- data.frame(Vintage = "1997", phen_stage = "3", wal_prcp_syrah = NA)
wal3.98 <- data.frame(Vintage = "1998", phen_stage = "3", wal_prcp_syrah = NA)

agp_wal_phen_syrah = rbind(agp_wal_phen_syrah, wal1.95, wal1.96, wal1.97, wal1.98, wal3.98, wal2.95, wal2.96,
                          wal2.97, wal2.98, wal3.95, wal3.96, wal3.97)
agp_wal_phen_syrah <- agp_wal_phen_syrah[order(agp_wal_phen_syrah$Vintage), ]

#agp_wal_phen_mer
wal1.95 <- data.frame(Vintage = "1995", phen_stage = "1", wal_prcp_mer = NA)
wal1.96 <- data.frame(Vintage = "1996", phen_stage = "1", wal_prcp_mer = NA)
wal1.97 <- data.frame(Vintage = "1997", phen_stage = "1", wal_prcp_mer = NA)
wal1.98 <- data.frame(Vintage = "1998", phen_stage = "1", wal_prcp_mer = NA)
wal2.95 <- data.frame(Vintage = "1995", phen_stage = "2", wal_prcp_mer = NA)
wal2.96 <- data.frame(Vintage = "1996", phen_stage = "2", wal_prcp_mer = NA)
wal2.97 <- data.frame(Vintage = "1997", phen_stage = "2", wal_prcp_mer = NA)
wal2.98 <- data.frame(Vintage = "1998", phen_stage = "2", wal_prcp_mer = NA)
wal3.95 <- data.frame(Vintage = "1995", phen_stage = "3", wal_prcp_mer = NA)
wal3.96 <- data.frame(Vintage = "1996", phen_stage = "3", wal_prcp_mer = NA)
wal3.97 <- data.frame(Vintage = "1997", phen_stage = "3", wal_prcp_mer = NA)
wal3.98 <- data.frame(Vintage = "1998", phen_stage = "3", wal_prcp_mer = NA)

agp_wal_phen_mer = rbind(agp_wal_phen_mer, wal1.95, wal1.96, wal1.97, wal1.98, wal3.98, wal2.95, wal2.96,
                        wal2.97, wal2.98, wal3.95, wal3.96, wal3.97)
agp_wal_phen_mer <- agp_wal_phen_mer[order(agp_wal_phen_mer$Vintage), ]


#agp_dayt_phen_cab
dayt1.06 <- data.frame(Vintage = "2006", phen_stage = "1", dayt_prcp_cab = NA)
dayt2.06 <- data.frame(Vintage = "2006", phen_stage = "2", dayt_prcp_cab = NA)
dayt3.06 <- data.frame(Vintage = "2006", phen_stage = "3", dayt_prcp_cab = NA)
dayt3.05 <- data.frame(Vintage = "2005", phen_stage = "3", dayt_prcp_cab = NA)

agp_dayt_phen_cab = rbind(agp_dayt_phen_cab, dayt1.06, dayt2.06, dayt3.06, dayt3.05)
agp_dayt_phen_cab <- agp_dayt_phen_cab[order(agp_dayt_phen_cab$Vintage), ]

#agp_dayt_phen_syrah
dayt1.06 <- data.frame(Vintage = "2006", phen_stage = "1", dayt_prcp_syrah = NA)
dayt2.06 <- data.frame(Vintage = "2006", phen_stage = "2", dayt_prcp_syrah = NA)
dayt3.06 <- data.frame(Vintage = "2006", phen_stage = "3", dayt_prcp_syrah = NA)
dayt3.05 <- data.frame(Vintage = "2005", phen_stage = "3", dayt_prcp_syrah = NA)

agp_dayt_phen_syrah = rbind(agp_dayt_phen_syrah, dayt1.06, dayt2.06, dayt3.06, dayt3.05)
agp_dayt_phen_syrah <- agp_dayt_phen_syrah[order(agp_dayt_phen_syrah$Vintage), ]

#agp_dayt_phen_mer
dayt1.06 <- data.frame(Vintage = "2006", phen_stage = "1", dayt_prcp_mer = NA)
dayt2.06 <- data.frame(Vintage = "2006", phen_stage = "2", dayt_prcp_mer = NA)
dayt3.06 <- data.frame(Vintage = "2006", phen_stage = "3", dayt_prcp_mer = NA)
dayt3.05 <- data.frame(Vintage = "2005", phen_stage = "3", dayt_prcp_mer = NA)

agp_dayt_phen_mer = rbind(agp_dayt_phen_mer, dayt1.06, dayt2.06, dayt3.06, dayt3.05)
agp_dayt_phen_mer <- agp_dayt_phen_mer[order(agp_dayt_phen_mer$Vintage), ]

#agp_hat_phen_cab
hat3.13 <- data.frame(Vintage = "2013", phen_stage = "3", hat_prcp_cab = NA)
hat3.14 <- data.frame(Vintage = "2014", phen_stage = "3", hat_prcp_cab = NA)
hat2.13 <- data.frame(Vintage = "2013", phen_stage = "2", hat_prcp_cab = NA)
hat1.13 <- data.frame(Vintage = "2013", phen_stage = "1", hat_prcp_cab = NA)

agp_hat_phen_cab = rbind(agp_hat_phen_cab, hat3.13, hat2.13, hat1.13, hat3.14)
agp_hat_phen_cab <- agp_hat_phen_cab[order(agp_dayt_phen_mer$Vintage), ]

#agp_hat_phen_syrah
hat3.13 <- data.frame(Vintage = "2013", phen_stage = "3", hat_prcp_syrah = NA)
hat3.14 <- data.frame(Vintage = "2014", phen_stage = "3", hat_prcp_syrah = NA)
hat2.13 <- data.frame(Vintage = "2013", phen_stage = "2", hat_prcp_syrah = NA)
hat1.13 <- data.frame(Vintage = "2013", phen_stage = "1", hat_prcp_syrah = NA)

agp_hat_phen_syrah = rbind(agp_hat_phen_syrah, hat3.13, hat2.13, hat1.13, hat3.14)
agp_hat_phen_syrah <- agp_hat_phen_syrah[order(agp_dayt_phen_mer$Vintage), ]

#agp_hat_phen_mer
hat3.13 <- data.frame(Vintage = "2013", phen_stage = "3", hat_prcp_mer = NA)
hat3.14 <- data.frame(Vintage = "2014", phen_stage = "3", hat_prcp_mer = NA)
hat2.13 <- data.frame(Vintage = "2013", phen_stage = "2", hat_prcp_mer = NA)
hat1.13 <- data.frame(Vintage = "2013", phen_stage = "1", hat_prcp_mer = NA)

agp_hat_phen_mer = rbind(agp_hat_phen_mer, hat3.13, hat2.13, hat1.13, hat3.14)
agp_hat_phen_mer <- agp_hat_phen_mer[order(agp_dayt_phen_mer$Vintage), ]


prcp_agg_phen <- merge(agp_hat_phen_cab, agp_dayt_phen_cab)
prcp_agg_phen <- merge(prcp_agg_phen, agp_wal_phen_cab)
prcp_agg_phen <- merge(prcp_agg_phen, agp_eph_phen_cab)
prcp_agg_phen <- merge(prcp_agg_phen, agp_dalle_phen_cab)
prcp_agg_phen <- merge(prcp_agg_phen, agp_dayt_phen_syrah)
prcp_agg_phen <- merge(prcp_agg_phen, agp_hat_phen_syrah)
prcp_agg_phen <- merge(prcp_agg_phen, agp_wal_phen_syrah)
prcp_agg_phen <- merge(prcp_agg_phen, agp_eph_phen_syrah)
prcp_agg_phen <- merge(prcp_agg_phen, agp_dalle_phen_syrah)
prcp_agg_phen <- merge(prcp_agg_phen, agp_dayt_phen_mer)
prcp_agg_phen <- merge(prcp_agg_phen, agp_hat_phen_mer)
prcp_agg_phen <- merge(prcp_agg_phen, agp_wal_phen_mer)
prcp_agg_phen <- merge(prcp_agg_phen, agp_eph_phen_mer)
prcp_agg_phen <- merge(prcp_agg_phen, agp_dalle_phen_mer)

#averaging by location for each variety
prcp_agg_phen$prcp_avg_phen_cab <- rowMeans(prcp_agg_phen[, 3:7], na.rm = TRUE)
prcp_agg_phen$prcp_avg_phen_syrah <- rowMeans(prcp_agg_phen[, 8:12], na.rm = TRUE)
prcp_agg_phen$prcp_avg_phen_mer <- rowMeans(prcp_agg_phen[, 13:17], na.rm = TRUE)

#By Year

#cabernet
agp_hat_year_cab <- aggregate(hat$prcpbase_cab, by = list(hat$year), FUN=sum) 
colnames(agp_hat_year_cab)[1] <- c("Vintage")
colnames(agp_hat_year_cab)[2] <- c("hat_prcp_cab")

agp_dayt_year_cab <- aggregate(dayt$prcpbase_cab, by = list(dayt$year), FUN=sum) 
colnames(agp_dayt_year_cab)[1] <- c("Vintage")
colnames(agp_dayt_year_cab)[2] <- c("dayt_prcp_cab")

agp_wal_year_cab <- aggregate(wal$prcpbase_cab, by = list(wal$year), FUN=sum) 
colnames(agp_wal_year_cab)[1] <- c("Vintage")
colnames(agp_wal_year_cab)[2] <- c("wal_prcp_cab")

agp_eph_year_cab <- aggregate(eph$prcpbase_cab, by = list(eph$year), FUN=sum) 
colnames(agp_eph_year_cab)[1] <- c("Vintage")
colnames(agp_eph_year_cab)[2] <- c("eph_prcp_cab")

agp_dalle_year_cab <- aggregate(dalle$prcpbase_cab, by = list(dalle$year), FUN=sum) 
colnames(agp_dalle_year_cab)[1] <- c("Vintage")
colnames(agp_dalle_year_cab)[2] <- c("dalle_prcp_cab")

#syrah
agp_hat_year_syrah <- aggregate(hat$prcpbase_syrah, by = list(hat$year), FUN=sum) 
colnames(agp_hat_year_syrah)[1] <- c("Vintage")
colnames(agp_hat_year_syrah)[2] <- c("hat_prcp_syrah")

agp_dayt_year_syrah <- aggregate(dayt$prcpbase_syrah, by = list(dayt$year), FUN=sum) 
colnames(agp_dayt_year_syrah)[1] <- c("Vintage")
colnames(agp_dayt_year_syrah)[2] <- c("dayt_prcp_syrah")

agp_wal_year_syrah <- aggregate(wal$prcpbase_syrah, by = list(wal$year), FUN=sum) 
colnames(agp_wal_year_syrah)[1] <- c("Vintage")
colnames(agp_wal_year_syrah)[2] <- c("wal_prcp_syrah")

agp_eph_year_syrah <- aggregate(eph$prcpbase_syrah, by = list(eph$year), FUN=sum) 
colnames(agp_eph_year_syrah)[1] <- c("Vintage")
colnames(agp_eph_year_syrah)[2] <- c("eph_prcp_syrah")

agp_dalle_year_syrah <- aggregate(dalle$prcpbase_syrah, by = list(dalle$year), FUN=sum) 
colnames(agp_dalle_year_syrah)[1] <- c("Vintage")
colnames(agp_dalle_year_syrah)[2] <- c("dalle_prcp_syrah")

#merlot
agp_hat_year_mer <- aggregate(hat$prcpbase_mer, by = list(hat$year), FUN=sum) 
colnames(agp_hat_year_mer)[1] <- c("Vintage")
colnames(agp_hat_year_mer)[2] <- c("hat_prcp_mer")

agp_dayt_year_mer <- aggregate(dayt$prcpbase_mer, by = list(dayt$year), FUN=sum) 
colnames(agp_dayt_year_mer)[1] <- c("Vintage")
colnames(agp_dayt_year_mer)[2] <- c("dayt_prcp_mer")

agp_wal_year_mer <- aggregate(wal$prcpbase_mer, by = list(wal$year), FUN=sum) 
colnames(agp_wal_year_mer)[1] <- c("Vintage")
colnames(agp_wal_year_mer)[2] <- c("wal_prcp_mer")

agp_eph_year_mer <- aggregate(eph$prcpbase_mer, by = list(eph$year), FUN=sum) 
colnames(agp_eph_year_mer)[1] <- c("Vintage")
colnames(agp_eph_year_mer)[2] <- c("eph_prcp_mer")

agp_dalle_year_mer <- aggregate(dalle$prcpbase_mer, by = list(dalle$year), FUN=sum) 
colnames(agp_dalle_year_mer)[1] <- c("Vintage")
colnames(agp_dalle_year_mer)[2] <- c("dalle_prcp_mer")

#editing stations with missing years
#agp_dalle_year_cab
dalle1.17 <- data.frame(Vintage = "2017", dalle_prcp_cab = NA)
dalle1.18 <- data.frame(Vintage = "2018", dalle_prcp_cab = NA)

agp_dalle_year_cab = rbind(agp_dalle_year_cab, dalle1.17, dalle1.18)
agp_dalle_year_cab <- agp_dalle_year_cab[order(agp_dalle_year_cab$Vintage), ]

#agp_dalle_year_syrah
dalle1.17 <- data.frame(Vintage = "2017", dalle_prcp_syrah = NA)
dalle1.18 <- data.frame(Vintage = "2018", dalle_prcp_syrah = NA)

agp_dalle_year_syrah = rbind(agp_dalle_year_syrah, dalle1.17, dalle1.18)
agp_dalle_year_syrah <- agp_dalle_year_syrah[order(agp_dalle_year_syrah$Vintage), ]

#agp_dalle_year_mer
dalle1.17 <- data.frame(Vintage = "2017", dalle_prcp_mer = NA)
dalle1.18 <- data.frame(Vintage = "2018", dalle_prcp_mer = NA)

agp_dalle_year_mer = rbind(agp_dalle_year_mer, dalle1.17, dalle1.18)
agp_dalle_year_mer <- agp_dalle_year_mer[order(agp_dalle_year_mer$Vintage), ]

#agp_eph_year_cab
agp_eph_year_cab$eph_prcp_cab[which(agp_eph_year_cab$Vintage=="2016")] <- NA 
eph1.17 <- data.frame(Vintage = "2017", eph_prcp_cab = NA)
eph1.18 <- data.frame(Vintage = "2018", eph_prcp_cab = NA)

agp_eph_year_cab = rbind(agp_eph_year_cab, eph1.17, eph1.18)
agp_eph_year_cab <- agp_eph_year_cab[order(agp_eph_year_cab$Vintage), ]

#agp_eph_year_syrah
agp_eph_year_syrah$eph_prcp_syrah[which(agp_eph_year_syrah$Vintage=="2016")] <- NA 
eph1.17 <- data.frame(Vintage = "2017", eph_prcp_syrah = NA)
eph1.18 <- data.frame(Vintage = "2018", eph_prcp_syrah = NA)

agp_eph_year_syrah = rbind(agp_eph_year_syrah, eph1.17, eph1.18)
agp_eph_year_syrah <- agp_eph_year_syrah[order(agp_eph_year_syrah$Vintage), ]

#agp_eph_year_mer
agp_eph_year_mer$eph_prcp_mer[which(agp_eph_year_mer$Vintage=="2016")] <- NA 
eph1.17 <- data.frame(Vintage = "2017", eph_prcp_mer = NA)
eph1.18 <- data.frame(Vintage = "2018", eph_prcp_mer = NA)

agp_eph_year_mer = rbind(agp_eph_year_mer, eph1.17, eph1.18)
agp_eph_year_mer <- agp_eph_year_mer[order(agp_eph_year_mer$Vintage), ]

#agp_wal_year_cab
wal1.95 <- data.frame(Vintage = "1995", wal_prcp_cab = NA)
wal1.96 <- data.frame(Vintage = "1996", wal_prcp_cab = NA)
wal1.97 <- data.frame(Vintage = "1997", wal_prcp_cab = NA)
wal1.98 <- data.frame(Vintage = "1998", wal_prcp_cab = NA)

agp_wal_year_cab = rbind(agp_wal_year_cab, wal1.95, wal1.96, wal1.97, wal1.98)
agp_wal_year_cab <- agp_wal_year_cab[order(agp_wal_year_cab$Vintage), ]

#agp_wal_year_syrah
wal1.95 <- data.frame(Vintage = "1995", wal_prcp_syrah = NA)
wal1.96 <- data.frame(Vintage = "1996", wal_prcp_syrah = NA)
wal1.97 <- data.frame(Vintage = "1997", wal_prcp_syrah = NA)
wal1.98 <- data.frame(Vintage = "1998", wal_prcp_syrah = NA)

agp_wal_year_syrah = rbind(agp_wal_year_syrah, wal1.95, wal1.96, wal1.97, wal1.98)
agp_wal_year_syrah <- agp_wal_year_syrah[order(agp_wal_year_syrah$Vintage), ]

#agp_wal_year_mer
wal1.95 <- data.frame(Vintage = "1995", wal_prcp_mer = NA)
wal1.96 <- data.frame(Vintage = "1996", wal_prcp_mer = NA)
wal1.97 <- data.frame(Vintage = "1997", wal_prcp_mer = NA)
wal1.98 <- data.frame(Vintage = "1998", wal_prcp_mer = NA)

agp_wal_year_mer = rbind(agp_wal_year_mer, wal1.95, wal1.96, wal1.97, wal1.98)
agp_wal_year_mer <- agp_wal_year_mer[order(agp_wal_year_mer$Vintage), ]

#agp_dayt_year_cab
dayt1.06 <- data.frame(Vintage = "2006", dayt_prcp_cab = NA)

agp_dayt_year_cab = rbind(agp_dayt_year_cab, dayt1.06)
agp_dayt_year_cab <- agp_dayt_year_cab[order(agp_dayt_year_cab$Vintage), ]

#agp_dayt_year_syrah
dayt1.06 <- data.frame(Vintage = "2006", dayt_prcp_syrah = NA)

agp_dayt_year_syrah = rbind(agp_dayt_year_syrah, dayt1.06)
agp_dayt_year_syrah <- agp_dayt_year_syrah[order(agp_dayt_year_syrah$Vintage), ]

#agp_dayt_phen_mer
dayt1.06 <- data.frame(Vintage = "2006", dayt_prcp_mer = NA)

agp_dayt_year_mer = rbind(agp_dayt_year_mer, dayt1.06)
agp_dayt_year_mer <- agp_dayt_year_mer[order(agp_dayt_year_mer$Vintage), ]

#agp_hat_phen_cab
hat1.13 <- data.frame(Vintage = "2013", hat_prcp_cab = NA)

agp_hat_year_cab = rbind(agp_hat_year_cab, hat1.13)
agp_hat_year_cab <- agp_hat_year_cab[order(agp_dayt_year_mer$Vintage), ]

#agp_hat_phen_syrah
hat1.13 <- data.frame(Vintage = "2013", hat_prcp_syrah = NA)

agp_hat_year_syrah = rbind(agp_hat_year_syrah, hat1.13)
agp_hat_year_syrah <- agp_hat_year_syrah[order(agp_dayt_year_mer$Vintage), ]

#agp_hat_phen_mer
hat1.13 <- data.frame(Vintage = "2013", hat_prcp_mer = NA)

agp_hat_year_mer = rbind(agp_hat_year_mer, hat1.13)
agp_hat_year_mer <- agp_hat_year_mer[order(agp_dayt_year_mer$Vintage), ]

prcp_agg_year <- merge(agp_hat_year_cab, agp_dayt_year_cab)
prcp_agg_year <- merge(prcp_agg_year, agp_wal_year_cab)
prcp_agg_year <- merge(prcp_agg_year, agp_eph_year_cab)
prcp_agg_year <- merge(prcp_agg_year, agp_dalle_year_cab)
prcp_agg_year <- merge(prcp_agg_year, agp_hat_year_syrah)
prcp_agg_year <- merge(prcp_agg_year, agp_dayt_year_syrah)
prcp_agg_year <- merge(prcp_agg_year, agp_wal_year_syrah)
prcp_agg_year <- merge(prcp_agg_year, agp_eph_year_syrah)
prcp_agg_year <- merge(prcp_agg_year, agp_dalle_year_syrah)
prcp_agg_year <- merge(prcp_agg_year, agp_hat_year_mer)
prcp_agg_year <- merge(prcp_agg_year, agp_dayt_year_mer)
prcp_agg_year <- merge(prcp_agg_year, agp_wal_year_mer)
prcp_agg_year <- merge(prcp_agg_year, agp_eph_year_mer)
prcp_agg_year <- merge(prcp_agg_year, agp_dalle_year_mer)

#averaging by location for each variety
prcp_agg_year$prcp_avg_year_cab <- rowMeans(prcp_agg_year[, 2:6], na.rm = TRUE)
prcp_agg_year$prcp_avg_year_syrah <- rowMeans(prcp_agg_year[, 7:11], na.rm = TRUE)
prcp_agg_year$prcp_avg_year_mer <- rowMeans(prcp_agg_year[, 12:16], na.rm = TRUE)

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
Syrah_Table <- Syrah_Table[,-c(2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,19,20,21,22,23,24,25,26,27,28,29,30,31,32,33,34,35,37)] #keeping averages
colnames(Syrah_Table)[3] <- c("var_gdd_avg_year")
colnames(Syrah_Table)[2] <- c("var_prcp_avg_year")

Merlot_Table <- merge(Agg_Table_year, Merlot)
Merlot_Table <- Merlot_Table[,-c(2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18,20,21,22,23,24,25,26,27,28,29,30,31,32,33,34,35,36)] #keeping averages
colnames(Merlot_Table)[2] <- c("var_prcp_avg_year")
colnames(Merlot_Table)[3] <- c("var_gdd_avg_year")

Cabernet_Table <- merge(Agg_Table_year, Cabernet) #values from gen
Cabernet_Table <- Cabernet_Table[,-c(2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,18,19,20,21,22,23,24,25,26,27,28,29,30,31,32,33,34,36,37)] #keeping averages
colnames(Cabernet_Table)[2] <- c("var_prcp_avg_year")
colnames(Cabernet_Table)[3] <- c("var_gdd_avg_year")

WA_Table_Full_Year <- rbind(Syrah_Table, Merlot_Table, Cabernet_Table)
WA_Table_Full_Year <- WA_Table_Full_Year[,c(1,4,5,6,7,8,9,2,3,10,11)] #reorganize columns

#phen
Syrah_Table <- merge(Agg_Table_phen, Syrah) #values from gen
Syrah_Table <- Syrah_Table[,-c(3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18,20,21,22,23,24,25,26,27,28,29,30,31,32,33,34,35,36,38)] #keeping averages
colnames(Syrah_Table)[4] <- c("var_gdd_avg_phen")
colnames(Syrah_Table)[3] <- c("var_prcp_avg_phen")

Merlot_Table <- merge(Agg_Table_phen, Merlot)
Merlot_Table <- Merlot_Table[,-c(3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18,19,21,22,23,24,25,26,27,28,29,30,31,32,33,34,35,36,37)] #keeping averages
colnames(Merlot_Table)[4] <- c("var_gdd_avg_phen")
colnames(Merlot_Table)[3] <- c("var_prcp_avg_phen")

Cabernet_Table <- merge(Agg_Table_phen, Cabernet) #values from gen
Cabernet_Table <- Cabernet_Table[,-c(3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,19,20,21,22,23,24,25,26,27,28,29,30,31,32,33,34,35,37,38)] #keeping averages
colnames(Cabernet_Table)[4] <- c("var_gdd_avg_phen")
colnames(Cabernet_Table)[3] <- c("var_prcp_avg_phen")

WA_Table_Full_Phen <- rbind(Syrah_Table, Merlot_Table, Cabernet_Table)
WA_Table_Full_Phen <- pivot_wider(WA_Table_Full_Phen, names_from = phen_stage, values_from = c(var_prcp_avg_phen, var_gdd_avg_phen))
WA_Table_Full_Phen <- WA_Table_Full_Phen[,c(1,2,3,4,5,6,7,10,11,12,13,14,15,8,9)] #reorganize columns

#Exporting as csv for future modeling
write.csv(WA_Table_Full_Year,"/Users/phoebeautio/Desktop/Vintages/data/NapaClimate2021/TablesForModels/WAComplete_year.csv", row.names = FALSE)
write.csv(WA_Table_Full_Phen,"/Users/phoebeautio/Desktop/Vintages/data/NapaClimate2021/TablesForModels/WAComplete_phen.csv", row.names = FALSE)
