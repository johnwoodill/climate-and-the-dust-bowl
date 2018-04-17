library(tidyverse)
library(ggmap)
library(RcppRoll)
library(noncensus)
library(maps)
library(grid)
library(lfe)
library(haven)
library(gganimate)

source("R/predictFelm.R")

data(zip_codes)
zip_codes <- dplyr::select(zip_codes, fips, latitude, longitude)
zip_codes <- zip_codes[!duplicated(zip_codes[,1:3]),]
names(zip_codes) <- c("fips", "lat", "long")
zip_codes <- zip_codes %>% 
  group_by(fips) %>% 
  summarise(lat = mean(lat, na.rm = TRUE),
            long = mean(long, na.rm = TRUE))

# dat <- read_csv("data/corn_1910-2016.csv")
# dat <- left_join(dat, zip_codes, by = c("fips"))
# dat$decade <- dat$year - (dat$year %% 10)
# dat <- filter(dat, year >= 1930)
# dat <- filter(dat, abs(long) <= 100)
# # dat <- filter(dat, fips %in% gp_fips)
# dat$year <- as.numeric(dat$year)

dat <- readRDS("data/full_ag_data.rds")
dat$decade <- dat$year - (dat$year %% 10)

# Aggregate county-level degree days -----------------------------------------------

# Full Data
# dd <- read_csv("data/fips_degree_days_1900-2013.csv")

# Condensed Data

# dd <- readRDS("data/sub_fips_degree_days_1900-2013.rds")
# prec <- read_csv("data/fips_precipitation_1900-2013.csv")
# 
# # dd <- read_dta("data/ddayByYearandFips_noweights.dta")
# 
# dd$year <- as.integer(dd$year)
# dd$fips <- as.integer(dd$fips)
# dd_dat <- left_join(dd, prec, by = c("fips", "year", "month"))
# 
# dd_dat <- dd_dat %>%
#   filter(month >= 3 & month <= 8) %>%
#   group_by(fips, year) %>%
#   summarise_all(sum) %>%
#   mutate(dday0_10 = dday0C - dday10C,
#          dday10_30 = dday10C - dday30C,
#          dday30 = dday30C,
#          prec = ppt,
#          prec_sq = ppt^2) %>%
#   dplyr::select(fips, year, dday0_10, dday10_30, dday30, prec, prec_sq) %>%
#   ungroup()

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
# 
# # Lag one so current year is not included
# dd_dat <- dd_dat %>%
#   group_by(fips) %>%
#   arrange(-year) %>%
#   mutate(dday0_10_lag1 = lag(dday0_10),
#          dday10_30_lag1 = lag(dday10_30),
#          dday30_lag1 = lag(dday30),
#          prec_lag1 = lag(prec))
# 
# dd_dat <- dd_dat %>%
#   group_by(fips) %>%
#   arrange(year) %>%
#   mutate(dday0_10_rm10 = roll_mean(dday0_10_lag1, 10, align = "right", fill = "NA"),
#          dday10_30_rm10 = roll_mean(dday10_30_lag1, 10, align = "right", fill = "NA"),
#          dday30_rm10 = roll_mean(dday30_lag1, 10, align = "right", fill = "NA"),
#          prec_rm10 = roll_mean(prec_lag1, 10, align = "right", fill = "NA"),
#          prec_sq_rm10 = prec_rm10^2) %>%
#   ungroup()
# 
# dat <- left_join(dat, dd_dat, by = c("fips", "year"))
dat$trend <- dat$year - (min(dat$year - 1))
dat$trend_sq <- dat$trend^2
# dat$corn_yield <- (dat$corn_grain_p/dat$corn_grain_a)
# dat$ln_corn_yield <- log(1 + dat$corn_yield)
dat$ln_corn_grain_a <- log(dat$corn_grain_a)
dat <- filter(dat, !is.na(corn_grain_a))
dat <- as.data.frame(dat)
# 
dat <- dat[complete.cases(dat),]
# 
# rm(dd_dat)

mod <- felm(ln_corn_grain_a ~ dday0_10 + dday10_30 + dday30 + prec + prec_sq + 
                dday0_10_rm10 + dday10_30_rm10 + dday30_rm10 + prec_rm10 + prec_sq_rm10 +
              trend + trend_sq
            | fips | 0 | 0, 
           data = dat, psdef=FALSE, weights = dat$corn_grain_a)
summary(mod)

# pmod <- predictFelm(mod, newdata = dat)

fe <- as.data.frame(getfe(mod))
fe <- select(fe, idx, effect)
row.names(fe) <- NULL
names(fe) <- c("fips", "effect")
fe$fips <- as.numeric(as.character(fe$fips))

# newdata <- dat %>%
#   group_by(fips) %>%
#   mutate(dday0_10 = mean(dday0_10),
#          dday10_30 = mean(dday10_30),
#          dday30 = mean(dday30),
#          prec = mean(prec),
#          prec_sq = mean(prec_sq)) %>% 
#   ungroup()

# newdata = as.data.frame(mod$X)

# pmod <- predictFelm(mod, newdata = newdata,
                    # var.terms = c("dday0_10_rm10", "dday10_30_rm10", "dday30_rm10", "prec_rm10", "prec_sq_rm10"))

cf <- as.matrix(mod$coefficients)[1:10]
modmat <- dplyr::select(dat, dday0_10, dday10_30, dday30, prec, prec_sq,
                        dday0_10_rm10, dday10_30_rm10, dday30_rm10, prec_rm10, prec_sq_rm10)
modmat <- as.matrix(modmat)

fit <- modmat %*% cf

fitdat <- data.frame(year = dat$year,
                     state = dat$state,
                     fips = dat$fips, 
                     pred = fit,
                     residuals = as.numeric(mod$residuals))

fitdat <- left_join(fitdat, zip_codes, by = c("fips"))
fitdat <- left_join(fitdat, fe, by = c("fips"))
fitdat$decade <- fitdat$year - (fitdat$year %% 10)
head(fitdat)
fitdat$fit <- exp(fitdat$pred + fitdat$effect)
head(fitdat)
head(dat$corn_grain_a)

# Centroid calculations
fitdat <- fitdat %>% 
  group_by(year) %>% 
  mutate(X_it = fit * long,
         Y_it = fit * lat) %>% 
  summarise(X = sum(X_it, na.rm = TRUE)/(sum(fit, na.rm = TRUE)),
            Y = sum(Y_it, na.rm = TRUE)/(sum(fit, na.rm = TRUE))) %>% 
  ungroup() %>% 
  # group_by(state) %>%
  mutate(X_lead = lead(X),
         Y_lead = lead(Y)) %>% 
  ungroup()
head(fitdat)

dat <- as.data.frame(dat)

usa_center = as.numeric(geocode("USA"))
USAMap <- ggmap(get_googlemap(center=usa_center, scale=2, zoom=5), extent="normal")
USAMap + geom_point(aes(x=X, y = Y, color = factor(year)), size = 0.5, data = fitdat) +
  geom_segment(data = fitdat, aes(x = X, y = Y, xend = X_lead, yend = Y_lead, color=factor(year)), arrow = arrow(length=unit(0.15,"cm")))



map <- get_googlemap(center=usa_center, scale=2, zoom=5)
p <- ggmap(map) +
  geom_point(dat, aes(x=long, y=lat, fill=ln_corn_yield, frame=year)) +
  scale_fill_distiller(palette = "Spectral")
p
gganimate(p, interval = 0.05, filename = "figures/animate_corn_yield.gif")



ggmap(map) +
  geom_point(data=filter(dat, year == 19), aes(x=long, y=lat, fill=ln_corn_yield))
