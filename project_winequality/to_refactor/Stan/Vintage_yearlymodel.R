
#Load libraries
library(rstan)
library(shinystan)

#Set cores and browser
options(mc.cores = 4)
options(browser = "chromium")
        
## Read data
napa_phen <- read.csv("../TablesForModels/NapaComplete_year.csv",header = TRUE)
sonoma_phen <- read.csv("../TablesForModels/UniqueSonomaComplete_year.csv", header = TRUE)
## nc_phen <- read.csv("../TablesForModels/NorthCoastComplete_year.csv", header = TRUE)
or_phen <- read.csv("../TablesForModels/ORComplete_year.csv", header = TRUE)
sb_phen <- read.csv("../TablesForModels/SBComplete_year.csv", header = TRUE)
wa_phen <- read.csv("../TablesForModels/WAComplete_year.csv", header = TRUE)
## Combine into 1 table
all_phen <- rbind(napa_phen, sonoma_phen, or_phen, sb_phen, wa_phen)

## ## quick fixes
## all_phen <- subset(all_phen, !(Vintage == 1991))
all_phen[which(all_phen$Location == "Santa Barbara "), "Location"]  <- c("Santa Barbara")

## Split off "General" Variety
gen_phen <- subset(all_phen, Variety == "General")
all_phen <- subset(all_phen, Variety != "General")

## Create location and variety indices
### Locations
locs <- unique(all_phen$Location)
locs <- locs[order(locs)]
locs.number <- as.numeric(as.factor(locs))
### Varieties
varieties <- unique(all_phen$Variety)
varieties <- varieties[order(varieties)]
varieties.number <- as.numeric(as.factor(varieties))

## Create variable with average ranks
avgranks <- all_phen$Avg_Rank
avgranks2 <- gen_phen$Avg_Rank

## Organize data for model fitting
data.stan <- list(N = nrow(all_phen),
                  avg_rank = all_phen$Avg_Rank,
                  n_location = length(locs),
                  location = as.numeric(as.factor(all_phen$Location)),
                  n_variety = length(varieties),
                  variety = as.numeric(as.factor(all_phen$Variety)),
                  precip = all_phen$var_prcp_avg_year,
                  gdd = all_phen$var_gdd_avg_year,
                  ## General data
                  N_gen = nrow(gen_phen),
                  location_gen = rep(3, nrow(gen_phen)), # Santa Barbara (ad hoc for now)
                  avg_rank_gen = gen_phen$Avg_Rank,
                  precip_gen = gen_phen$var_prcp_avg_year,
                  gdd_gen = gen_phen$var_gdd_avg_year)

## Fit Stan model
fit1 <- stan("yearly.stan",
             data = data.stan,
             iter = 2000,
             warmup = 1000,
             chains = 4)

## Rename locations and varieties
names(fit1)[5:9] <- locs
names(fit1)[10:16] <- varieties

## View diagnostics
launch_shinystan(fit1)

## Summarize posterior samples
summary(fit1, pars = c("base_rank", "a_location", "sigma_location", "a_variety", "sigma_variety", "sigma_rank", "b_precip", "b_gdd"))$summary[, "mean"]

pdf(file = "Results_yearly.pdf", onefile = TRUE)
plot(fit1, pars = c("a_location"))
plot(fit1, pars = c("a_variety"))
plot(fit1, pars = c("b_gdd", "b_precip"))
plot(fit1, pars = c("sigma_location", "sigma_variety", "sigma_rank"))
dev.off()
