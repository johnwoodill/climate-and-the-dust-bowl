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

dat <- readRDS("data/full_ag_data.rds")

# Raw data centroid
dat$fit <- dat$ln_farmland

# Centroid calculations
dat <- dat %>% 
  group_by(decade) %>% 
  mutate(X_it = fit * long,
         Y_it = fit * lat) %>% 
  summarise(X = sum(X_it, na.rm = TRUE)/(sum(fit, na.rm = TRUE)),
            Y = sum(Y_it, na.rm = TRUE)/(sum(fit, na.rm = TRUE))) %>% 
  # ungroup() %>% 
  # group_by(state) %>%
  mutate(X_lead = lead(X),
         Y_lead = lead(Y)) %>% 
  ungroup()

head(dat)

dat <- as.data.frame(dat)

usa_center = as.numeric(geocode("USA"))
USAMap <- ggmap(get_googlemap(center=usa_center, scale=2, zoom=5), extent="normal")
USAMap + geom_point(aes(x=X, y = Y, color = factor(decade)), size = 0.5, data = dat) +
  geom_segment(data = dat, aes(x = X, y = Y, xend = X_lead, yend = Y_lead, color=factor(decade)), 
               arrow = arrow(length=unit(0.15,"cm")))



# Fit weather/climate variation
dat <- select(dat, fips, state, ln_cropland, dday10_30, dday30, prec, prec_sq, dday10_30_rm10, dday30_rm10, prec_rm10, prec_sq_rm10, trend )

mod <- felm(ln_farmland ~ dday10_30 + dday30 + prec + prec_sq + 
                dday10_30_rm10 + dday30_rm10 + prec_rm10 + prec_sq_rm10 +
              year + I(year^2)
            | fips | 0 | 0, 
           data = dat, psdef=FALSE)
summary(mod)

fe <- as.data.frame(getfe(mod))
fe <- select(fe, idx, effect)
row.names(fe) <- NULL
names(fe) <- c("fips", "effect")
fe$fips <- as.numeric(as.character(fe$fips))

cf <- as.matrix(mod$coefficients)[1:8]
modmat <- dplyr::select(dat, dday10_30, dday30, prec, prec_sq,
                        dday10_30_rm10, dday30_rm10, prec_rm10, prec_sq_rm10)
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
fitdat$fit <- exp(fitdat$pred + fitdat$effect + fitdat$residuals)
head(fitdat)
head(dat$cropland)

dat$fit <- dat$ln_farmland
dat <- filter(dat, !is.na(fit))
fitdat <- dat
# fitdat <- filter(fitdat, year == 1910)

# Centroid calculations
fitdat <- fitdat %>% 
  group_by(year) %>% 
  mutate(X_it = fit * long,
         Y_it = fit * lat) %>% 
  group_by(year) %>% 
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


# animated GIF
map <- get_googlemap(center=usa_center, scale=2, zoom=5)
p <- ggmap(map) +
  geom_point(dat, aes(x=long, y=lat, fill=ln_corn_yield, frame=year)) +
  scale_fill_distiller(palette = "Spectral")
p
gganimate(p, interval = 0.05, filename = "figures/animate_corn_yield.gif")



ggmap(map) +
  geom_point(data=filter(dat, year == 19), aes(x=long, y=lat, fill=ln_corn_yield))
