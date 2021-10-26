

daymet <- read.csv("data/Climate/DaymetMeans/climDailyMeans.csv", header = TRUE)

napa <- subset(daymet, Region == "napa")

head(napa)

head(napa[order(napa$date), ])

napa <- napa[order(napa$date), ]

napa$Year <- as.numeric(strftime(strptime(napa$date,format="%Y-%m-%d"), format = "%Y")) # format = %j for DOY

napa.1980 <- subset(napa, Year == 1980)

napa.1980
