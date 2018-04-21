library(ggplot2)
library(ggthemes)
library(choroplethr)
library(tidyverse)
library(RcppRoll)
library(lfe)
library(maps)
library(haven)


source("R/misc_functions.R")

cropdat <- readRDS("data/full_ag_data.rds")
cropdat$state <- factor(cropdat$state)
cropdat <- filter(cropdat, !is.na(corn_grain_a))
cropdat$decade <- cropdat$year - (cropdat$year %% 10)
cropdat$twenty <- cropdat$year - (cropdat$year %% 20)

# Great Plains Region
co <- c(8001,8005,8009,8011,8013,8017,8019,8025,8027,8031,8035,8039,8041,8043,8047,8055,8059,8061,8063,8069,8071,8073,8075,8087,8089,8093,8095,8099,8101,8115,8119,8121,8123,8125)
ks <- c(20007,20009,20023,20025,20033,20039,20047,20051,20053,20055,20057,20063,20065,20067,20069,20071,20075,20077,20081,20083,20089,20093,20095,20097,20101,20105,20109,20119,20123,20129,20135,20137,20141,20145,20147,20151,20153,20155,20159,20163,20165,20167,20171,20175,20179,20181,20183,20185,20187,20189,20193,20195,20199,20203)
ne <- c(31001,31003,31005,31007,31009,31011,31013,31015,31017,31019,31027,31029,31031,31033,31035,31041,31043,31045,31047,31049,31051,31057,31061,31063,31065,31069,31071,31073,31075,31077,31079,31081,31083,31085,31087,31089,31091,31093,31099,31101,31103,31105,31107,31111,31113,31115,31117,31119,31121,31123,31125,31129,31135,31137,31139,31145,31149,31157,31161,31163,31165,31167,31171,31173,31175,31179,31181,31183)
nm <- c(35005,35007,35009,35011,35015,35019,35021,35025,35033,35037,35041,35047,35059)
ok <- c(40003,40007,40009,40011,40015,40017,40025,40031,40033,40039,40043,40045,40047,40051,40053,40055,40057,40059,40065,40067,40073,40075,40093,40129,40137,40139,40141,40149,40151,40153)
tx <- c(48003,48009,48011,48017,48023,48033,48045,48059,48065,48069,48075,48077,48079,48087,48101,48107,48111,48115,48117,48125,48129,48133,48143,48151,48153,48155,48165,48169,48179,48189,48191,48195,48197,48205,48207,48211,48219,48221,48227,48233,48237,48253,48263,48269,48275,48279,48295,48303,48305,48317,48335,48337,48341,48345,48353,48357,48359,48363,48367,48369,48375,48381,48393,48415,48417,48421,48429,48433,48437,48441,48445,48447,48483,48485,48487,48497,48501,48503)
mt <- c(30003,30005,30009,30011,30013,30015,30017,30019,30021,30025,30027,30033,30037,30041,30045,30049,30051,30055,30059,30065,30069,30071,30073,30075,30079,30083,30085,30087,30091,30095,30097,30099,30101,30103,30105,30107,30109,30111)
nd <- c(38001,38003,38005,38007,38009,38011,38013,38015,38017,38019,38021,38023,38025,38027,38029,38031,38033,38035,38037,38039,38041,38043,38045,38047,38049,38051,38053,38055,38057,38059,38061,38063,38065,38067,38069,38071,38073,38075,38077,38079,38081,38083,38085,38087,38089,38091,38093,38095,38097,38099,38101,38103,38105)
sd <- c(46003,46005,46007,46009,46011,46013,46015,46017,46019,46021,46023,46025,46027,46029,46031,46033,46035,46037,46039,46041,46043,46045,46047,46049,46051,46053,46055,46057,46059,46061,46063,46065,46067,46069,46071,46073,46075,46077,46079,46081,46083,46085,46087,46089,46091,46093,46095,46097,46099,46101,46103,46105,46107,46109,46111,46113,46115,46117,46119,46121,46123,46125,46127,46129,46135,46137)
wy <- c(56003,56005,56009,56011,56015,46017,56019,56021,56027,56031,56033,56043,56045)
gp_fips <- c(co, ks, ne, nm, ok, tx, mt, nd, sd, wy)
ngp_fips <- c(ne, mt, nd, sd, wy)
db_fips <- c(tx, ok, ks, co, nm)

cm <- select(cropdat, year, fips, state, corn_yield)
cm <- filter(cm, fips %in% gp_fips)
cm <- filter(cm, year == 1930)
cm <- select(cm, fips, corn_yield)
names(cm) <- c("fips", "value")
check_map(cm$fips, cm$value) + theme(legend.position = "none")
ggsave("figures/map_counties.pdf", width = 6, height = 6)

cropdat <- filter(cropdat, fips %in% gp_fips)
cropdat <- filter(cropdat, year >= 1910 & year <= 2010)
cropdat$region <- ifelse(cropdat$fips %in% db_fips, 1, 0)

cropdat <- cropdat %>% 
  group_by(fips) %>% 
  mutate(dday0_10_dm = dday0_10 - mean(dday0_10, na.rm = TRUE),
         dday10_30_dm = dday10_30 - mean(dday10_30, na.rm = TRUE),
         dday30_dm = dday30 - mean(dday30, na.rm = TRUE),
         prec_dm = prec - mean(prec, na.rm = TRUE),
         prec_sq_dm = prec_dm^2,
         corn_yield_dm = corn_yield - mean(corn_yield, na.rm = TRUE),
         # dday30_rm10_dm = dday30_rm10 - mean(dday30_rm10, na.rm = TRUE),
         prec_dm = prec - mean(prec, na.rm = TRUE),
         ln_corn_yield = log(1 + corn_yield),
         ln_corn_yield_dm = ln_corn_yield - mean(ln_corn_yield, na.rm = TRUE),
         ln_farmland = log(1 + farmland),
         ln_cropland = log(1 + cropland),
         ln_farmland_dm = ln_farmland - mean(ln_farmland, na.rm = TRUE),
         # value_crops_dm = value_crops - mean(value_crops, na.rm = TRUE),
         # ln_value_crops = log(1 + value_crops),
         # value_landbuildings_dm = value_landbuildings - mean(value_landbuildings, na.rm = TRUE),
         # ln_value_landbuildings = log(1 + value_landbuildings),
         w = roll_mean(corn_grain_a, 3, .01)) %>% 
  filter(ln_corn_yield < 10) %>% 
  ungroup()


regdat <- filter(cropdat, !is.na(corn_grain_a))

fit <- felm(ln_cropland ~ dday10_30 + dday30 + prec + prec_sq + 
              # dday10_30_rm10 + dday30_rm10 + prec_rm10 + prec_sq_rm10 +
              state:trend + state:I(trend^2) | fips | 0 | state, 
            data = regdat, psdef=FALSE)
summary(fit)

# Corn yield
ggplot(cropdat, aes(year, corn_yield, color = factor(state))) + geom_smooth()

fit <- felm(ln_cropland ~ state:trend + state:trend_sq + 
              factor(decade):dday10_30 + factor(decade):dday30 + 
              factor(decade):prec + factor(decade):prec_sq | fips | 0 | 0, 
            data = cropdat)

summary(fit)

coef <- fit$coefficients[29:36]
se <- fit$se[29:36]

ggplot(NULL, aes(x = seq(1910, 1990, 10), y = coef)) + geom_point() + geom_line() +
  geom_errorbar(aes(ymin = coef - 1.96*se, ymax = coef + 1.96*se), width = .5) +
  scale_x_continuous(breaks = seq(1910, 1990, 10)) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "grey") +
  theme_tufte(base_size = 10) +
  xlab("Decade") +
  ylab("Degree Day (30C) Coefficient \n Weather-effect") +
  annotate("segment", x=-Inf, xend=Inf, y=-Inf, yend=-Inf, color = "grey") +
  annotate("segment", x=-Inf, xend=-Inf, y=-Inf, yend=Inf, color = "grey") +
  theme(legend.position = "none",
    # legend.justification = c("left", "top"),
    legend.box.background = element_rect(colour = "grey"),
    legend.title = element_blank(), legend.key = element_blank())


fit <- felm(ln_farmland~ state:trend + state:trend_sq + 
              factor(decade):dday10_30_rm10 + factor(decade):dday30_rm10 + 
              factor(decade):prec_rm10 + factor(decade):prec_sq_rm10 | fips | 0 | 0, 
            data = cropdat, weights = cropdat$corn_grain_a)

summary(fit)

coef <- fit$coefficients[29:37]
se <- fit$se[29:37]

ggplot(NULL, aes(x = seq(1910, 1990, 10), y = coef)) + geom_point() + geom_line() +
  geom_errorbar(aes(ymin = coef - 1.96*se, ymax = coef + 1.96*se), width = .5) +
  scale_x_continuous(breaks = seq(1910, 1990, 10)) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "grey") +
  theme_tufte(base_size = 10) +
  xlab("Decade") +
  ylab("Degree Day (30C) Coefficient \n Climate-effect (10-year)") +
  annotate("segment", x=-Inf, xend=Inf, y=-Inf, yend=-Inf, color = "grey") +
  annotate("segment", x=-Inf, xend=-Inf, y=-Inf, yend=Inf, color = "grey") +
  theme(legend.position = "none",
    # legend.justification = c("left", "top"),
    legend.box.background = element_rect(colour = "grey"),
    legend.title = element_blank(), legend.key = element_blank()) 


