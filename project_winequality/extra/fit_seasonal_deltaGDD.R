##
## This version converts GDDs to GDDs - 800, where 800 represents a kind of
## minimum threshold for "good" wine. Such an approach is one way to
## account for non-linear responses to GDD (z-score may also work)
##
## Unfinished as of March 22 2022
##

#Load libraries
library(rstan)
library(shinystan)

#Set cores and browser
options(mc.cores = 4)
options(browser = "chromium")
        
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

aggregate(Avg_Rank ~ Location * Variety, data = all.score, FUN = length)

## Add climate data to scores
all.score$deltaGDD_Season <- NA
all.score$Prcp_Season <- NA
for(i in 1:nrow(all.score)){
    temp <- subset(all.climate, Year == all.score$Vintage[i] & Location == all.score$Location[i])
    all.score$deltaGDD_Season[i] <- temp$GDD_Season - 800 ## chosen GDD threshold
    all.score$Prcp_Season[i] <- temp$Prcp_Season
}

## Create location and variety indices
### Locations
locs <- unique(all.score$Location)
locs <- locs[order(locs)]
locs.number <- as.numeric(as.factor(locs))

### Varieties
varieties <- unique(all.score$Variety)
varieties <- varieties[order(varieties)]
varieties.number <- as.numeric(as.factor(varieties))

## Organize data for model fitting
data.stan <- list(N = nrow(all.score),
                  avg_rank = all.score$Avg_Rank,
                  n_location = length(locs),
                  location = as.numeric(as.factor(all.score$Location)),
                  n_variety = length(varieties),
                  variety = as.numeric(as.factor(all.score$Variety)),
                  precip = all.score$Prcp_Season,
                  gdd = all.score$deltaGDD_Season)
## Fit Stan model
fit1 <- stan("project_winequality/stan/seasonal.stan",
             data = data.stan,
             iter = 2000,
             warmup = 1000,
             chains = 4,
             control = list(adapt_delta = 0.9)
             )

## Rename locations and varieties (recommend replacing direct indexing with grep)
names(fit1)[520:524] <- locs
names(fit1)[525:531] <- varieties

## View diagnostics
launch_shinystan(fit1)

## Summarize posterior samples
summary(fit1, pars = c("base_rank", "a_location", "sigma_location", "a_variety", "sigma_variety", "sigma_rank", "b_precip", "b_gdd"))$summary[, "mean"]


## Save estimates
saveRDS(object = fit1, file = "project_winequality/output/posterior_seasonal_deltaGDD.RDS")

## Make plots
pdf(file = "project_winequality/output/Results_seasonal_deltaGDD.pdf", onefile = TRUE)
plot(fit1, pars = c("rank_location"))
plot(fit1, pars = c("rank_variety"))
plot(fit1, pars = c("b_gdd", "b_precip"))
plot(fit1, pars = c("sigma_location", "sigma_variety", "sigma_rank"))
dev.off()
