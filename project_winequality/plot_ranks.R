
## Read climate data
all.climate <- read.csv("project_winequality/output/climate_allregions.csv", header = TRUE, stringsAsFactors = FALSE)

## Obtain vintage scores
score1 <- read.csv("project_winequality/data/NapaSonomaNC_Vintage.csv", header = TRUE)
score1 <- score1[, -c(8, 9)]
score1[which(score1$Location == "North Coast"), "Location"] <- c("North_Coast")
score2 <- read.csv("project_winequality/data/OR_Vintage.csv", header = TRUE)
score2 <- score2[, -c(8, 9)]
score2[which(score2$Location == "Oregon"), "Location"] <- c("Williamette")
score3 <- read.csv("project_winequality/data/WA_Vintage.csv", header = TRUE)
score3 <- score3[, -c(8, 9)]
score3[which(score3$Location == "Washington"), "Location"] <- c("Columbia")
all.score <- rbind(score1, score2, score3)

## Fix vintage label
all.score[which(all.score$Variety == "Cabernet "), "Variety"] <- c("Cabernet")
## Fix rank (according to latest WS download)
all.score[which(all.score$Variety == "Cabernet" & all.score$Location == "Napa" & all.score$Vintage == 2017), "R1_WS"] <- c(92)
all.score[which(all.score$Variety == "Zinfandel" & all.score$Location == "Napa" & all.score$Vintage == 2018), "R1_WS"] <- c(92)
all.score[which(all.score$Variety == "Zinfandel" & all.score$Location == "Sonoma" & all.score$Vintage == 2018), "R1_WS"] <- c(94)
all.score[which(all.score$Variety == "Rhône-Style Reds" & all.score$Location == "Napa" & all.score$Vintage == 2017), "R1_WS"] <- c(90)
all.score[which(all.score$Variety == "Rhône-Style Reds" & all.score$Location == "Sonoma" & all.score$Vintage == 2017), "R1_WS"] <- c(89)
## Fix averaging of ranks
all.score$Avg_Rank <- apply(all.score[, c("R1_WS", "R2_WE", "R3_WA")], MARGIN = 1, FUN = function(X) mean(X, na.rm = TRUE))
## Remove NAs
all.score <- subset(all.score, Avg_Rank > 0 & Vintage > 1979)

###
### Plots for summarizing data
###

## Plot ranks of Merlot
## Make plots for visualizing
pdf(file = "project_winequality/output/Ranks_Merlot.pdf", width = 8, height = 6)
merlot.sub <- subset(all.score, Variety == "Merlot")
loc.sub <- unique(merlot.sub$Location)
xrange = c(1980, 2020)
yrange = c(70, 100)
plot(NA,
     xlim = xrange,
     ylim = yrange,
     xlab = "Year", ylab = "Average Rank",
     main = "Merlot")
for(i in 1:length(loc.sub)){
    temp <- subset(merlot.sub, Location == loc.sub[i])
    temp <- temp[order(temp$Vintage), ]
    points(Avg_Rank ~ Vintage, data = temp, type = "l", col = i + 3, lwd = 3)
}
legend("topleft", legend = loc.sub, col = c(4:6), lty = "solid", inset = 0.04, lwd = 2)
dev.off()

## Plot ranks of Chardonnay
## Make plots for visualizing
pdf(file = "project_winequality/output/Ranks_Chardonnay.pdf", width = 8, height = 6)
chardonnay.sub <- subset(all.score, Variety == "Chardonnay")
loc.sub <- unique(chardonnay.sub$Location)
xrange = c(1980, 2020)
yrange = c(70, 100)
plot(NA,
     xlim = xrange,
     ylim = yrange,
     xlab = "Year", ylab = "Average Rank",
     main = "Chardonnay")
for(i in 1:length(loc.sub)){
    temp <- subset(chardonnay.sub, Location == loc.sub[i])
    temp <- temp[order(temp$Vintage), ]
    points(Avg_Rank ~ Vintage, data = temp, type = "l", col = i + 3, lwd = 3)
}
legend("topleft", legend = loc.sub, col = c(4:6), lty = "solid", inset = 0.04, lwd = 2)
dev.off()

## Plot ranks of Cabernet
## Make plots for visualizing
pdf(file = "project_winequality/output/Ranks_Cabernet.pdf", width = 8, height = 6)
cabernet.sub <- subset(all.score, Variety == "Cabernet")
loc.sub <- unique(cabernet.sub$Location)
xrange = c(1980, 2020)
yrange = c(70, 100)
plot(NA,
     xlim = xrange,
     ylim = yrange,
     xlab = "Year", ylab = "Average Rank",
     main = "Cabernet")
for(i in 1:length(loc.sub)){
    temp <- subset(cabernet.sub, Location == loc.sub[i])
    temp <- temp[order(temp$Vintage), ]
    points(Avg_Rank ~ Vintage, data = temp, type = "l", col = i + 3, lwd = 3)
}
legend("bottomright", legend = loc.sub, col = c(4:7), lty = "solid", inset = 0.04, lwd = 2)
dev.off()

## Plot ranks of Syrah
## Make plots for visualizing
pdf(file = "project_winequality/output/Ranks_Syrah.pdf", width = 8, height = 6)
syrah.sub <- subset(all.score, Variety == "Syrah")
loc.sub <- unique(syrah.sub$Location)
xrange = c(1980, 2020)
yrange = c(70, 100)
plot(NA,
     xlim = xrange,
     ylim = yrange,
     xlab = "Year", ylab = "Average Rank",
     main = "Syrah")
for(i in 1:length(loc.sub)){
    temp <- subset(syrah.sub, Location == loc.sub[i])
    temp <- temp[order(temp$Vintage), ]
    points(Avg_Rank ~ Vintage, data = temp, type = "l", col = i + 3, lwd = 3)
}
legend("bottomright", legend = loc.sub, col = c(4:7), lty = "solid", inset = 0.04, lwd = 2)
dev.off()

## Plot ranks of Zinfandel
## Make plots for visualizing
pdf(file = "project_winequality/output/Ranks_Zinfandel.pdf", width = 8, height = 6)
zinfandel.sub <- subset(all.score, Variety == "Zinfandel")
loc.sub <- unique(zinfandel.sub$Location)
xrange = c(1980, 2020)
yrange = c(70, 100)
plot(NA,
     xlim = xrange,
     ylim = yrange,
     xlab = "Year", ylab = "Average Rank",
     main = "Zinfandel")
for(i in 1:length(loc.sub)){
    temp <- subset(zinfandel.sub, Location == loc.sub[i])
    temp <- temp[order(temp$Vintage), ]
    points(Avg_Rank ~ Vintage, data = temp, type = "l", col = i + 3, lwd = 3)
}
legend("topleft", legend = loc.sub, col = c(4:7), lty = "solid", inset = 0.04, lwd = 2)
dev.off()

## Plot ranks of Pinot Noir
## Make plots for visualizing
pdf(file = "project_winequality/output/Ranks_Pinot_Noir.pdf", width = 8, height = 6)
pinotnoir.sub <- subset(all.score, Variety == "Pinot Noir")
loc.sub <- unique(pinotnoir.sub$Location)
xrange = c(1980, 2020)
yrange = c(65, 100)
plot(NA,
     xlim = xrange,
     ylim = yrange,
     xlab = "Year", ylab = "Average Rank",
     main = "Pinot Noir")
for(i in 1:length(loc.sub)){
    temp <- subset(pinotnoir.sub, Location == loc.sub[i])
    temp <- temp[order(temp$Vintage), ]
    points(Avg_Rank ~ Vintage, data = temp, type = "l", col = i + 3, lwd = 3)
}
legend("topleft", legend = loc.sub, col = c(4:7), lty = "solid", inset = 0.04, lwd = 2)
dev.off()

## Plot ranks of Rhône-Style Red
## Make plots for visualizing
pdf(file = "project_winequality/output/Ranks_Rhone.pdf", width = 8, height = 6)
rhone.sub <- subset(all.score, Variety == "Rhône-Style Reds")
loc.sub <- unique(rhone.sub$Location)
xrange = c(1980, 2020)
yrange = c(70, 100)
plot(NA,
     xlim = xrange,
     ylim = yrange,
     xlab = "Year", ylab = "Average Rank",
     main = "Rhône-Style Red")
for(i in 1:length(loc.sub)){
    temp <- subset(rhone.sub, Location == loc.sub[i])
    temp <- temp[order(temp$Vintage), ]
    points(Avg_Rank ~ Vintage, data = temp, type = "l", col = i + 3, lwd = 3)
}
legend("topleft", legend = loc.sub, col = c(4:7), lty = "solid", inset = 0.04, lwd = 2)
dev.off()
