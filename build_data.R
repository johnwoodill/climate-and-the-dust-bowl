library(tidyverse)
library(RcppRoll)
library(rms)
library(noncensus)
library(maps)
library(lubridate)
library(stringr)
library(foreign)
library(haven)
library(lfe)
library(zoo)

dummyCreator <- function(invec, prefix = NULL) {
     L <- length(invec)
     ColNames <- sort(unique(invec))
     M <- matrix(0L, ncol = length(ColNames), nrow = L,
                 dimnames = list(NULL, ColNames))
     M[cbind(seq_len(L), match(invec, ColNames))] <- 1L
     if (!is.null(prefix)) colnames(M) <- paste(prefix, colnames(M), sep = "_")
     M
}

# Function to extract data
data(county.fips) 
county.fips$state <- sapply(str_split(county.fips$polyname, ","),'[',1)
county.fips$county <- sapply(str_split(county.fips$polyname, ","),'[',2)
county.fips <- dplyr::select(county.fips, fips, county, state)

data(zip_codes)
zip_codes <- dplyr::select(zip_codes, fips, latitude, longitude)
zip_codes <- zip_codes[!duplicated(zip_codes[,1:3]),]
names(zip_codes) <- c("fips", "lat", "long")
zip_codes <- zip_codes %>% 
  group_by(fips) %>% 
  summarise(lat = mean(lat, na.rm = TRUE),
            long = mean(long, na.rm = TRUE))

# Get all combinations of years and states
data(states)
stateabb <- dplyr::select(states, state, name)
stateabb$state <- tolower(stateabb$state)
stateabb$name <- tolower(stateabb$name)

# Aggregate county level ag data ------------------------------------------
# 
# # Load crop data (balanced years 1927-2007)
# corn <- read_csv("data/corn_1910-2016.csv")
# corn$state <- tolower(corn$state)
# corn$fips <- as.integer(corn$fips)
# 
# cotton <- read_csv("data/cotton_1919-2016.csv")
# cotton$state <- tolower(cotton$state)
# cotton$fips <- as.integer(cotton$fips)
# 
# hay <- read_csv("data/hay_1918-2008.csv")
# hay$state <- tolower(hay$state)
# hay$fips <- as.integer(hay$fips)
# 
# wheat <- read_csv("data/wheat_1909-2007.csv")
# # wheat <- read_csv("data/wheat_1909-2007_spring.csv")
# wheat$state <- tolower(wheat$state)
# wheat$fips <- as.integer(wheat$fips)
# 
# soybean <- read_csv("data/soybean_1927-2016.csv")
# soybean$state <- tolower(soybean$state)
# soybean$fips <- as.integer(soybean$fips)
# 
# # Get all combinations of years and states
# newgrid <- expand.grid(county.fips$fips, 1900:2016)#
# mergdat <- data.frame(county = county.fips$fips, name = county.fips$state)
# statedat <- select(states, state, name)
# statedat$state <- tolower(statedat$state)
# statedat$name <- tolower(statedat$name)
# mergdat <- left_join(mergdat, statedat, by = c("name"))
# mergdat <- select(mergdat, county, state)
# names(newgrid) <- c("county", "year")
# newgrid <- left_join(newgrid, mergdat, by = "county")
# newgrid <- left_join(newgrid, zip_codes, by = "county")
# newgrid <- newgrid[!duplicated(newgrid[,1:3]),]
# newgrid$county <- as.character(newgrid$county)
# names(newgrid) <- c("fips", "year", "state", "lat", "long")
# newgrid$fips <- as.integer(newgrid$fips)
# #
# # # Merge crop data
# cropdat <- left_join(newgrid, corn, by = c("year", "state", "fips"))
# cropdat <- left_join(cropdat, cotton, by = c("state", "fips", "year"))
# cropdat <- left_join(cropdat, hay, by = c("state", "fips", "year"))
# cropdat <- left_join(cropdat, wheat, by = c("state", "fips", "year"))
# cropdat <- left_join(cropdat, soybean, by = c("state", "fips", "year"))
# 


#-----------------------------------------------------
# Merge historical Haines data
hdat <- read_dta("data/DustBowl_All_base1910.dta")
hdat <- dplyr::select(hdat, year, fips, corn_grain_a, corn_grain_y, cotton_a, cotton_y,
               hay_a, hay_y, wheat_a, wheat_y, value_landbuildings, value_crops)
# hdat[hdat == 0] <- NA

hdat$year <- as.integer(hdat$year)
hdat$fips <- as.integer(hdat$fips)

names(hdat)[3:12] <- c("corn_grain_a", "corn_grain_p", "cotton_a", "cotton_p", "hay_a",
                       "hay_p", "wheat_a", "wheat_p", "value_landbuildings", "value_crops")

hdat <- left_join(hdat, zip_codes, by = c("fips"))
cropdat <- hdat
cropdat$year <- as.numeric(cropdat$year)
head(cropdat)
#-----------------------------------------------------

# Aggregate county-level degree days -----------------------------------------------

# Full Data
# dd <- read_csv("data/fips_degree_days_1900-2013.csv")

# Condensed Data

dd <- readRDS("data/sub_fips_degree_days_1900-2013.rds")
prec <- read_csv("data/fips_precipitation_1900-2013.csv")

dd$year <- as.integer(dd$year)
dd$fips <- as.integer(dd$fips)
dd_dat <- left_join(dd, prec, by = c("fips", "year", "month"))

dd_dat <- dd_dat %>%
  filter(month >= 3 & month <= 8) %>%
  group_by(fips, year) %>%
  summarise_all(sum) %>%
  mutate(dday0_10 = dday0C - dday10C,
         dday10_30 = dday10C - dday30C,
         dday30 = dday30C,
         prec = ppt,
         prec_sq = ppt^2) %>%
  dplyr::select(fips, year, dday0_10, dday10_30, dday30, prec, prec_sq) %>%
  ungroup()

#  
# dd <- read_dta("data/FULL_ddayByYearandFips_noweights.dta")
# 
# dd_dat <- dd %>%
#   # filter(month >= 3 & month <= 10) %>%
#   # group_by(fips, year) %>%
#   # summarise_all(sum) %>%
#   mutate(dday0_10 = dday0C - dday10C,
#          dday10_30 = dday10C - dday30C,
#          dday30 = dday30C,
#          prec = prec,
#          prec_sq = prec^2) %>%
#   select(fips, year, dday0_10, dday10_30, dday30, prec, prec_sq) %>%
#   ungroup()

#--------------------------------------------------
# Roll.mean intervals

# Lag one so current year is not included
dd_dat <- dd_dat %>%
  group_by(fips) %>%
  arrange(-year) %>%
  mutate(dday0_10_lag1 = lag(dday0_10),
         dday10_30_lag1 = lag(dday10_30),
         dday30_lag1 = lag(dday30),
         prec_lag1 = lag(prec)) %>% 
  ungroup()

dd_dat <- dd_dat %>%
  group_by(fips) %>%
  arrange(year) %>%
  mutate(dday0_10_rm10 = roll_mean(dday0_10_lag1, 10, align = "right", fill = "NA"),
         dday10_30_rm10 = roll_mean(dday10_30_lag1, 10, align = "right", fill = "NA"),
         dday30_rm10 = roll_mean(dday30_lag1, 10, align = "right", fill = "NA"),
         prec_rm10 = roll_mean(prec_lag1, 10, align = "right", fill = "NA"),
         prec_sq_rm10 = prec_rm10^2) %>%
  ungroup()

fulldat <- left_join(cropdat, dd_dat, by = c("fips", "year"))

# Merge ag prices, ag crop data, and degree day data ----------------------
 
# Import region data file
region_dat <- read_csv("data/ResourceRegionCRDfips.csv")
names(region_dat) <- c("fips", "ers_region", "crd")
fulldat <- left_join(fulldat, region_dat, by = "fips")

# Yield
fulldat$corn_yield <- fulldat$corn_grain_p/fulldat$corn_grain_a
fulldat$cotton_yield <- (fulldat$cotton_p)/fulldat$cotton_a
fulldat$hay_yield <- fulldat$hay_p/fulldat$hay_a
fulldat$wheat_yield <- fulldat$wheat_p/fulldat$wheat_a

# Convert inf to NA
fulldat <- do.call(data.frame,lapply(fulldat, function(x) replace(x, is.infinite(x), 0)))

# Log crop revenue
fulldat$ln_corn_yield <- log(1 + fulldat$corn_yield)
fulldat$ln_cotton_yield <- log(1 + fulldat$cotton_yield)
fulldat$ln_hay_yield <- log(1 + fulldat$hay_yield)
fulldat$ln_wheat_yield <- log(1 + fulldat$wheat_yield)

# Remove inf to na
is.na(fulldat) <- do.call(cbind, lapply(fulldat, is.infinite))

# Build trends
fulldat$trend <- fulldat$year - min(fulldat$year)
fulldat$trend_sq <- fulldat$trend^2


# fulldat$trend_lat <- fulldat$trend*fulldat$lat
# fulldat$trend_long <- fulldat$trend*fulldat$long
# fulldat$trend_sq_long <- fulldat$trend_sq*fulldat$long
# fulldat$trend_sq_lat <- fulldat$trend_sq*fulldat$lat

# Save fulldat

fulldat <- filter(fulldat, abs(long) <= 100 )
fulldat$state <- factor(fulldat$state)

fulldat <- left_join(fulldat, county.fips, by = c("fips"))

decade_dummy <- dummyCreator(fulldat$year, prefix = "d")
fulldat <- cbind(fulldat, decade_dummy)

saveRDS(fulldat, "data/full_ag_data.rds")
fulldat <- readRDS("data/full_ag_data.rds")


regdat <- filter(fulldat, !is.na(corn_grain_a))

fit <- felm(ln_corn_yield ~ dday0_10 + dday10_30 + dday30 + prec + prec_sq + trend + trend_sq | fips | 0 | state, 
            data = regdat, weights = regdat$corn_grain_a)
summary(fit)

regdat <- regdat %>% 
  group_by(fips) %>% 
  mutate(ln_corn_yield_dm = ln_corn_yield - mean(ln_corn_yield, na.rm = TRUE)) %>% 
  ungroup()

ggplot(regdat, aes(y=ln_corn_yield_dm, x=dday10_30)) + geom_point() + geom_smooth(method = "lm")
