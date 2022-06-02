
## Read data
# setwd("~/Documents/git/projects/vinmisc/vintages")
daymet <- read.csv("project_winequality/data/Climate/DaymetMeans/climDailyMeans.csv", header = TRUE)

## Add Year, Month, Day, DOY
daymet$Year <- as.numeric(strftime(strptime(daymet$date,format="%Y-%m-%d"), format = "%Y"))
daymet$Month <- as.numeric(strftime(strptime(daymet$date,format="%Y-%m-%d"), format = "%m"))
daymet$Day <- as.numeric(strftime(strptime(daymet$date,format="%Y-%m-%d"), format = "%d"))
daymet$DOY <- as.numeric(strftime(strptime(daymet$date,format="%Y-%m-%d"), format = "%j")) # format = %j for DOY

## Day of Years corresponding (on average) to different phenological stages
doytable <- data.frame(Variety = c("General"),
                       Budburst_Start = c(91),
                       Budburst_End = c(135),
                       Bloom_Start = c(136),
                       Bloom_End = c(204),
                       Veraison_Start = c(205),
                       Veraison_End = c(258))

## Function for calculating GDD
gdd.calc <- function(X, Threshold, Cap){
    gdd <- dim(X)
    for(i in 1:length(X)){
        if(isTRUE(X[i] <= Threshold | X[i] >= Cap)){
            gdd[i] <- 0
        } else{
            gdd[i] <- X[i] - Threshold
        }
    }
    return(gdd)
}


## Napa
### Subset
napa <- subset(daymet, Region == "napa")
### Order
napa <- napa[order(napa$date), ]
### View
head(napa)
### What years are available?
years.napa <- unique(napa$Year)
### Obtain relevant climate data
stor.napa <- data.frame()
for(i in 1:length(years.napa)){
    temp <- subset(napa, Year == years.napa[i])
    temp <- temp[order(temp$date), ]
    stor.napa <- rbind(stor.napa, data.frame(Location = "Napa",
                                   Year = years.napa[i],
                                   GDD_Budburst = sum(gdd.calc(temp$Tmean[doytable$Budburst_Start[1]:doytable$Budburst_End[1]], Threshold = 10, Cap = 200)),
                                   GDD_Bloom = sum(gdd.calc(temp$Tmean[doytable$Bloom_Start[1]:doytable$Bloom_End[1]], Threshold = 10, Cap = 200)),
                                   GDD_Veraison = sum(gdd.calc(temp$Tmean[doytable$Veraison_Start[1]:doytable$Veraison_End[1]], Threshold = 10, Cap = 200)),
                                   Prcp_Budburst = sum(temp$Prcp[doytable$Budburst_Start[1]:doytable$Budburst_End[1]]),
                                   Prcp_Bloom = sum(temp$Prcp[doytable$Bloom_Start[1]:doytable$Bloom_End[1]]),
                                   Prcp_Veraison = sum(temp$Prcp[doytable$Veraison_Start[1]:doytable$Veraison_End[1]])))
}
### Obtain seasonal values
stor.napa$GDD_Season <- rowSums(stor.napa[, c("GDD_Budburst", "GDD_Bloom", "GDD_Veraison")])
stor.napa$Prcp_Season <- rowSums(stor.napa[, c("Prcp_Budburst", "Prcp_Bloom", "Prcp_Veraison")])
### View
head(stor.napa)

## Sonoma
### Subset
sonoma <- subset(daymet, Region == "sonoma")
### Order
sonoma <- sonoma[order(sonoma$date), ]
### View
head(sonoma)
### What years are available?
years.sonoma <- unique(sonoma$Year)
### Obtain relevant climate data
stor.sonoma <- data.frame()
for(i in 1:length(years.sonoma)){
    temp <- subset(sonoma, Year == years.sonoma[i])
    temp <- temp[order(temp$date), ]
    stor.sonoma <- rbind(stor.sonoma, data.frame(Location = "Sonoma",
                                   Year = years.sonoma[i],
                                   GDD_Budburst = sum(gdd.calc(temp$Tmean[doytable$Budburst_Start[1]:doytable$Budburst_End[1]], Threshold = 10, Cap = 200)),
                                   GDD_Bloom = sum(gdd.calc(temp$Tmean[doytable$Bloom_Start[1]:doytable$Bloom_End[1]], Threshold = 10, Cap = 200)),
                                   GDD_Veraison = sum(gdd.calc(temp$Tmean[doytable$Veraison_Start[1]:doytable$Veraison_End[1]], Threshold = 10, Cap = 200)),
                                   Prcp_Budburst = sum(temp$Prcp[doytable$Budburst_Start[1]:doytable$Budburst_End[1]]),
                                   Prcp_Bloom = sum(temp$Prcp[doytable$Bloom_Start[1]:doytable$Bloom_End[1]]),
                                   Prcp_Veraison = sum(temp$Prcp[doytable$Veraison_Start[1]:doytable$Veraison_End[1]])))
}
### Obtain seasonal values
stor.sonoma$GDD_Season <- rowSums(stor.sonoma[, c("GDD_Budburst", "GDD_Bloom", "GDD_Veraison")])
stor.sonoma$Prcp_Season <- rowSums(stor.sonoma[, c("Prcp_Budburst", "Prcp_Bloom", "Prcp_Veraison")])
### View
head(stor.sonoma)

## North_Coast
### Subset
ncoast <- subset(daymet, Region == "nCoast")
### Order
ncoast <- ncoast[order(ncoast$date), ]
### View
head(ncoast)
### What years are available?
years.ncoast <- unique(ncoast$Year)
### Obtain relevant climate data
stor.ncoast <- data.frame()
for(i in 1:length(years.ncoast)){
    temp <- subset(ncoast, Year == years.ncoast[i])
    temp <- temp[order(temp$date), ]
    stor.ncoast <- rbind(stor.ncoast, data.frame(Location = "North_Coast",
                                   Year = years.ncoast[i],
                                   GDD_Budburst = sum(gdd.calc(temp$Tmean[doytable$Budburst_Start[1]:doytable$Budburst_End[1]], Threshold = 10, Cap = 200)),
                                   GDD_Bloom = sum(gdd.calc(temp$Tmean[doytable$Bloom_Start[1]:doytable$Bloom_End[1]], Threshold = 10, Cap = 200)),
                                   GDD_Veraison = sum(gdd.calc(temp$Tmean[doytable$Veraison_Start[1]:doytable$Veraison_End[1]], Threshold = 10, Cap = 200)),
                                   Prcp_Budburst = sum(temp$Prcp[doytable$Budburst_Start[1]:doytable$Budburst_End[1]]),
                                   Prcp_Bloom = sum(temp$Prcp[doytable$Bloom_Start[1]:doytable$Bloom_End[1]]),
                                   Prcp_Veraison = sum(temp$Prcp[doytable$Veraison_Start[1]:doytable$Veraison_End[1]])))
}
### Obtain seasonal values
stor.ncoast$GDD_Season <- rowSums(stor.ncoast[, c("GDD_Budburst", "GDD_Bloom", "GDD_Veraison")])
stor.ncoast$Prcp_Season <- rowSums(stor.ncoast[, c("Prcp_Budburst", "Prcp_Bloom", "Prcp_Veraison")])
### View
head(stor.ncoast)

## Williamette Valley (Oregon)
### Subset
williamette <- subset(daymet, Region == "willamet")
### Order
williamette <- williamette[order(williamette$date), ]
### View
head(williamette)
### What years are available?
years.williamette <- unique(williamette$Year)
### Obtain relevant climate data
stor.williamette <- data.frame()
for(i in 1:length(years.williamette)){
    temp <- subset(williamette, Year == years.williamette[i])
    temp <- temp[order(temp$date), ]
    stor.williamette <- rbind(stor.williamette, data.frame(Location = "Williamette",
                                   Year = years.williamette[i],
                                   GDD_Budburst = sum(gdd.calc(temp$Tmean[doytable$Budburst_Start[1]:doytable$Budburst_End[1]], Threshold = 10, Cap = 200)),
                                   GDD_Bloom = sum(gdd.calc(temp$Tmean[doytable$Bloom_Start[1]:doytable$Bloom_End[1]], Threshold = 10, Cap = 200)),
                                   GDD_Veraison = sum(gdd.calc(temp$Tmean[doytable$Veraison_Start[1]:doytable$Veraison_End[1]], Threshold = 10, Cap = 200)),
                                   Prcp_Budburst = sum(temp$Prcp[doytable$Budburst_Start[1]:doytable$Budburst_End[1]]),
                                   Prcp_Bloom = sum(temp$Prcp[doytable$Bloom_Start[1]:doytable$Bloom_End[1]]),
                                   Prcp_Veraison = sum(temp$Prcp[doytable$Veraison_Start[1]:doytable$Veraison_End[1]])))
}
### Obtain seasonal values
stor.williamette$GDD_Season <- rowSums(stor.williamette[, c("GDD_Budburst", "GDD_Bloom", "GDD_Veraison")])
stor.williamette$Prcp_Season <- rowSums(stor.williamette[, c("Prcp_Budburst", "Prcp_Bloom", "Prcp_Veraison")])
### View
head(stor.williamette)

## Columbia Valley (Washington)
### Subset
columbia <- subset(daymet, Region == "columbia")
### Order
columbia <- columbia[order(columbia$date), ]
### View
head(columbia)
### What years are available?
years.columbia <- unique(columbia$Year)
### Obtain relevant climate data
stor.columbia <- data.frame()
for(i in 1:length(years.columbia)){
    temp <- subset(columbia, Year == years.columbia[i])
    temp <- temp[order(temp$date), ]
    stor.columbia <- rbind(stor.columbia, data.frame(Location = "Columbia",
                                   Year = years.columbia[i],
                                   GDD_Budburst = sum(gdd.calc(temp$Tmean[doytable$Budburst_Start[1]:doytable$Budburst_End[1]], Threshold = 10, Cap = 200)),
                                   GDD_Bloom = sum(gdd.calc(temp$Tmean[doytable$Bloom_Start[1]:doytable$Bloom_End[1]], Threshold = 10, Cap = 200)),
                                   GDD_Veraison = sum(gdd.calc(temp$Tmean[doytable$Veraison_Start[1]:doytable$Veraison_End[1]], Threshold = 10, Cap = 200)),
                                   Prcp_Budburst = sum(temp$Prcp[doytable$Budburst_Start[1]:doytable$Budburst_End[1]]),
                                   Prcp_Bloom = sum(temp$Prcp[doytable$Bloom_Start[1]:doytable$Bloom_End[1]]),
                                   Prcp_Veraison = sum(temp$Prcp[doytable$Veraison_Start[1]:doytable$Veraison_End[1]])))
}
### Obtain seasonal values
stor.columbia$GDD_Season <- rowSums(stor.columbia[, c("GDD_Budburst", "GDD_Bloom", "GDD_Veraison")])
stor.columbia$Prcp_Season <- rowSums(stor.columbia[, c("Prcp_Budburst", "Prcp_Bloom", "Prcp_Veraison")])
### View
head(stor.columbia)

## Combine all regions and save
comb.all <- rbind(stor.napa, stor.sonoma, stor.ncoast, stor.williamette, stor.columbia)
### Save
write.table(x = comb.all, file = "project_winequality/output/climate_allregions.csv", sep = ",", row.names = FALSE)

## Make plots for visualizing
ymin <- 750
ymax <- 1750
pdf(file = "project_winequality/output/SeasonalGDD.pdf", width = 12, height = 8)
par(mfrow = c(2, 3))
plot(GDD_Season ~ Year, data = stor.napa,
     ylim = c(ymin, ymax),
     xlab = "Year", ylab = "GDD > 10", type = "l", main = "Napa Valley (California)", col = "magenta4")
points(GDD_Season ~ Year, data = stor.napa, type = "p", pch = 21, col = "black", bg = "magenta4")
plot(GDD_Season ~ Year, data = stor.sonoma,
     ylim = c(ymin, ymax),
     xlab = "Year", ylab = "GDD > 10", type = "l", main = "Sonoma Valley (California)", col = "magenta4")
points(GDD_Season ~ Year, data = stor.sonoma, type = "p", pch = 21, col = "black", bg = "magenta4")
plot(GDD_Season ~ Year, data = stor.ncoast,
     ylim = c(ymin, ymax),
     xlab = "Year", ylab = "GDD > 10", type = "l", main = "North Coast (California)", col = "magenta4")
points(GDD_Season ~ Year, data = stor.ncoast, type = "p", pch = 21, col = "black", bg = "magenta4")
plot(GDD_Season ~ Year, data = stor.williamette,
     ylim = c(ymin, ymax),
     xlab = "Year", ylab = "GDD > 10", type = "l", main = "Williamette Valley (Oregon)", col = "magenta4")
points(GDD_Season ~ Year, data = stor.williamette, type = "p", pch = 21, col = "black", bg = "magenta4")
plot(GDD_Season ~ Year, data = stor.columbia,
     ylim = c(ymin, ymax),
     xlab = "Year", ylab = "GDD > 10", type = "l", main = "Columbia Valley (Washington)", col = "magenta4")
points(GDD_Season ~ Year, data = stor.columbia, type = "p", pch = 21, col = "black", bg = "magenta4")
dev.off()
