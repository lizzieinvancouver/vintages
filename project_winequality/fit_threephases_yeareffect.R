
#Load libraries
library(rstan)
library(shinystan)

#Set cores and browser
options(mc.cores = 4)
options(browser = "chromium")
        
## Read climate data
all.climate <- read.csv("project_winequality/output/climate_allregions.csv", header = TRUE, stringsAsFactors = FALSE)
## Vintage scores
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
all.score$GDD_Budburst <- NA
all.score$Prcp_Budburst <- NA
all.score$GDD_Flowering <- NA
all.score$Prcp_Flowering <- NA
all.score$GDD_Veraison <- NA
all.score$Prcp_Veraison <- NA
for(i in 1:nrow(all.score)){
    temp <- subset(all.climate, Year == all.score$Vintage[i] & Location == all.score$Location[i])
    all.score$GDD_Budburst[i] <- temp$GDD_Budburst
    all.score$Prcp_Budburst[i] <- temp$Prcp_Budburst
    all.score$GDD_Flowering[i] <- temp$GDD_Bloom
    all.score$Prcp_Flowering[i] <- temp$Prcp_Bloom
    all.score$GDD_Veraison[i] <- temp$GDD_Veraison
    all.score$Prcp_Veraison[i] <- temp$Prcp_Veraison
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
                  precip1 = all.score$Prcp_Budburst,
                  precip2 = all.score$Prcp_Flowering,
                  precip3 = all.score$Prcp_Veraison,
                  gdd1 = all.score$GDD_Budburst,
                  gdd2 = all.score$GDD_Flowering,
                  gdd3 = all.score$GDD_Veraison,
                  year = as.numeric(as.factor(all.score$Vintage)))
## Fit Stan model
fit1 <- stan("project_winequality/stan/threephases_yeareffect.stan",
             data = data.stan,
             iter = 2000,
             warmup = 1000,
             chains = 4)

## Rename locations and varieties
names(fit1)[525:529] <- locs
names(fit1)[530:536] <- varieties

## View diagnostics
launch_shinystan(fit1)

## Save estimates
saveRDS(object = fit1, file = "project_winequality/output/posterior_threephases_yeareffect.RDS")

## Summarize posterior samples
summary(fit1, pars = c("base_rank", "a_location", "sigma_location", "a_variety", "sigma_variety", "sigma_rank", "b_precip1", "b_precip2", "b_precip3", "b_gdd1", "b_gdd2", "b_gdd3", "b_year"))$summary[, "mean"]

pdf(file = "project_winequality/output/Results_threephases_yeareffect.pdf", onefile = TRUE)
plot(fit1, pars = c("rank_location"))
plot(fit1, pars = c("rank_variety"))
plot(fit1, pars = c("b_gdd1", "b_gdd2", "b_gdd3"))
plot(fit1, pars = c("b_precip1", "b_precip2", "b_precip3"))
plot(fit1, pars = c("b_year"))
plot(fit1, pars = c("sigma_location", "sigma_variety", "sigma_rank"))
dev.off()
