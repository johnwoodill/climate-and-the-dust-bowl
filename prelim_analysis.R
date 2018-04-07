library(ggplot2)
library(ggthemes)
library(choroplethr)
library(tidyverse)
library(RcppRoll)
library(lfe)

source("R/misc_functions.R")

cropdat <- readRDS("data/full_ag_data.rds")
cropdat$state <- factor(cropdat$state)
cropdat <- filter(cropdat, !is.na(corn_grain_a))
cropdat$decade <- cropdat$year - (cropdat$year %% 10)

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
db_fips <- c(tx, ok, ks, co, nm)

cm <- select(cropdat, year, fips, state, corn_yield)
cm <- filter(cm, fips %in% gp_fips)
cm <- filter(cm, year == 1930)
cm <- select(cm, fips, corn_yield)
names(cm) <- c("fips", "value")
check_map(cm) + theme(legend.position = "none")
ggsave("figures/map_counties.pdf", width = 6, height = 6)

cropdat <- filter(cropdat, fips %in% gp_fips)
cropdat <- filter(cropdat, year >= 1910 & year <= 2010)

cropdat <- cropdat %>% 
  group_by(fips) %>% 
  mutate(dday0_10_dm = dday0_10 - mean(dday0_10, na.rm = TRUE),
         dday10_30_dm = dday10_30 - mean(dday10_30, na.rm = TRUE),
         dday30_dm = dday30 - mean(dday30, na.rm = TRUE),
         prec_dm = prec - mean(prec, na.rm = TRUE),
         prec_sq_dm = prec_dm^2,
         corn_yield_dm = corn_yield - mean(corn_yield, na.rm = TRUE),
         dday30_rm10_dm = dday30_rm10 - mean(dday30_rm10, na.rm = TRUE),
         prec_dm = prec - mean(prec, na.rm = TRUE),
         ln_corn_yield = log(1 + corn_yield),
         ln_corn_yield_dm = ln_corn_yield - mean(ln_corn_yield, na.rm = TRUE),
         # value_crops_dm = value_crops - mean(value_crops, na.rm = TRUE),
         # ln_value_crops = log(1 + value_crops),
         # value_landbuildings_dm = value_landbuildings - mean(value_landbuildings, na.rm = TRUE),
         # ln_value_landbuildings = log(1 + value_landbuildings),
         w = roll_mean(corn_grain_a, 3, .01)) %>% 
  filter(ln_corn_yield < 10) %>% 
  ungroup()

# Weather
ggplot(cropdat, aes(y=ln_corn_yield, x=dday0_10, color = state)) + geom_point(alpha = 0.25) + geom_smooth(method = "lm")
ggplot(cropdat, aes(ln_corn_yield, dday10_30_dm, color = state)) + geom_point(alpha = 0.25) + geom_smooth(method = "lm")
ggplot(cropdat, aes(ln_corn_yield, dday30_dm, color = state)) + geom_point(alpha = 0.25) + geom_smooth(method = "lm")
ggplot(cropdat, aes(log(1+corn_yield), prec_dm, color = state)) + geom_point(alpha = 0.25) + geom_smooth(method = "lm")

#----------------------------------------------------------------------
# Corn Yield
ggplot(cropdat, aes(year, corn_yield_dm, color = state)) + geom_smooth() +
  geom_hline(yintercept = 0, linetype = "dashed", color = "grey") +
  theme_tufte(base_size = 12) +
  xlab(NULL) +
  ylab("Corn Yield (w/ County FE)") +
  annotate("segment", x=-Inf, xend=Inf, y=-Inf, yend=-Inf, color = "grey") +
  annotate("segment", x=-Inf, xend=-Inf, y=-Inf, yend=Inf, color = "grey") +
  scale_x_continuous(breaks = seq(1910, 2010, 10)) +
  theme(legend.position = "right",
    # legend.justification = c("left", "top"),
    legend.box.background = element_rect(colour = "grey"),
    legend.title = element_blank(), legend.key = element_blank()) 
ggsave("figures/corn_yield_ts.pdf", width = 6, height = 4) 

ggplot(cropdat, aes(y = ln_corn_yield_dm, x = dday30)) + 
  theme_tufte(base_size = 10) +
  xlab("Degree Day 30C") +
  ylab("Log(Corn Yield) w/ County FE") +
  annotate("segment", x=-Inf, xend=Inf, y=-Inf, yend=-Inf, color = "grey") +
  annotate("segment", x=-Inf, xend=-Inf, y=-Inf, yend=Inf, color = "grey") +
  geom_point(alpha = 0.25) + 
  geom_smooth(method = "lm") + 
  theme(legend.position = "none",
    # legend.justification = c("left", "top"),
    legend.box.background = element_rect(colour = "grey"),
    legend.title = element_blank(), legend.key = element_blank()) +
  facet_wrap(~decade)
ggsave("figures/ln_corn_yield_dday30.pdf", width = 6, height = 4)

ggplot(cropdat, aes(y = ln_corn_yield_dm, x = dday10_30)) + 
  theme_tufte(base_size = 10) +
  xlab("Degree Day 10-30C") +
  ylab("Log(Corn Yield) w/ County FE") +
  annotate("segment", x=-Inf, xend=Inf, y=-Inf, yend=-Inf, color = "grey") +
  annotate("segment", x=-Inf, xend=-Inf, y=-Inf, yend=Inf, color = "grey") +
  geom_point(alpha = 0.25) + 
  geom_smooth(method = "lm") + 
  theme(legend.position = "none",
    # legend.justification = c("left", "top"),
    legend.box.background = element_rect(colour = "grey"),
    legend.title = element_blank(), legend.key = element_blank()) +
  facet_wrap(~decade)
ggsave("figures/ln_corn_yield_dday10_30.pdf", width = 6, height = 4)



#----------------------------------------------------------------------
# Ag Revenue
ggplot(cropdat, aes(year, value_crops_dm, color = state)) + geom_smooth() +
  geom_hline(yintercept = 0, linetype = "dashed", color = "grey") +
  theme_tufte(base_size = 12) +
  xlab(NULL) +
  ylab("Value of Crops (w/ County FE)") +
  annotate("segment", x=-Inf, xend=Inf, y=-Inf, yend=-Inf, color = "grey") +
  annotate("segment", x=-Inf, xend=-Inf, y=-Inf, yend=Inf, color = "grey") +
  scale_x_continuous(breaks = seq(1910, 2010, 10)) +
  theme(legend.position = "right",
    # legend.justification = c("left", "top"),
    legend.box.background = element_rect(colour = "grey"),
    legend.title = element_blank(), legend.key = element_blank()) 
ggsave("figures/value_crops_ts.pdf", width = 6, height = 4) 

ggplot(cropdat, aes(y = ln_value_crops_dm, x = dday30)) + 
  theme_tufte(base_size = 10) +
  xlab("Degree Day 30C") +
  ylab("Log(Corn Yield)") +
  annotate("segment", x=-Inf, xend=Inf, y=-Inf, yend=-Inf, color = "grey") +
  annotate("segment", x=-Inf, xend=-Inf, y=-Inf, yend=Inf, color = "grey") +
  geom_point(alpha = 0.25) + 
  geom_smooth(method = "lm") + 
  theme(legend.position = "none",
    # legend.justification = c("left", "top"),
    legend.box.background = element_rect(colour = "grey"),
    legend.title = element_blank(), legend.key = element_blank()) +
  facet_wrap(~decade)
ggsave("figures/ln_value_crops_dday30.pdf", width = 6, height = 4)

#----------------------------------------------------------------------
# Farmland Value
ggplot(cropdat, aes(year, value_landbuildings_dm, color = state)) + geom_smooth() +
  geom_hline(yintercept = 0, linetype = "dashed", color = "grey") +
  theme_tufte(base_size = 12) +
  xlab(NULL) +
  ylab("Farmland Values (w/ County FE)") +
  annotate("segment", x=-Inf, xend=Inf, y=-Inf, yend=-Inf, color = "grey") +
  annotate("segment", x=-Inf, xend=-Inf, y=-Inf, yend=Inf, color = "grey") +
  scale_x_continuous(breaks = seq(1910, 2010, 10)) +
  theme(legend.position = "right",
    # legend.justification = c("left", "top"),
    legend.box.background = element_rect(colour = "grey"),
    legend.title = element_blank(), legend.key = element_blank()) 
ggsave("figures/value_landbuildings_ts.pdf", width = 6, height = 4) 

ggplot(cropdat, aes(y = ln_value_landbuildings, x = dday30)) + 
  theme_tufte(base_size = 10) +
  xlab("Degree Day 30C") +
  ylab("Log(Farmland Values)") +
  annotate("segment", x=-Inf, xend=Inf, y=-Inf, yend=-Inf, color = "grey") +
  annotate("segment", x=-Inf, xend=-Inf, y=-Inf, yend=Inf, color = "grey") +
  geom_point(alpha = 0.25) + 
  geom_smooth(method = "lm") + 
  theme(legend.position = "none",
    # legend.justification = c("left", "top"),
    legend.box.background = element_rect(colour = "grey"),
    legend.title = element_blank(), legend.key = element_blank()) +
  facet_wrap(~decade)
ggsave("figures/ln_value_landbuildings_dday30.pdf", width = 6, height = 4)


# Climate
ggplot(cropdat, aes(corn_yield, dday0_10_rm10, color = state)) + geom_point(alpha = 0.25) + geom_smooth(method = "lm")
ggplot(cropdat, aes(corn_yield, dday10_30_rm10, color = state)) + geom_point(alpha = 0.25) + geom_smooth(method = "lm")
ggplot(cropdat, aes(corn_yield, dday30_rm10, color = state)) + geom_point(alpha = 0.25) + geom_smooth(method = "lm")
ggplot(cropdat, aes(corn_yield, prec, color = state)) + geom_point(alpha = 0.25) + geom_smooth(method = "lm")
ggplot(cropdat, aes(corn_yield, prec^2, color = state)) + geom_point(alpha = 0.25) + geom_smooth(method = "lm")
ggplot(cropdat, aes(corn_yield, prec_rm10, color = state)) + geom_point(alpha = 0.25) + geom_smooth(method = "lm")
ggplot(cropdat, aes(corn_yield, prec_rm10^2, color = state)) + geom_point(alpha = 0.25) + geom_smooth(method = "lm")

# Corn yield
ggplot(cropdat, aes(year, corn_yield_dm, color = factor(state))) + geom_smooth()

# regdat <- filter(cropdat, state %in% c("ks", "nm", "co"))
fit <- felm(ln_corn_yield ~ dday0_10 + dday10_30 + dday30 + prec + prec_sq + 
             trend_lat + trend_long + trend_sq_lat + trend_sq_long | fips | 0 | state, 
           data = cropdat, weights = cropdat$w)
summary(fit)

outdat <- data.frame()
for (i in seq(1910, 2000, 10)){
  regdat <- filter(cropdat, year >= i & year <= i + 10)
  
  fit <- felm(log(value_landbuildings) ~ dday0_10 + dday10_30 + dday30 + prec + prec_sq + 
             trend_lat + trend_long + trend_sq_lat + trend_sq_long | fips | 0 | state, 
           data = regdat, psdef=FALSE, weights = regdat$w)
  coef <- 100*fit$coefficients[3]
  se <- 100*fit$se[3]
  indat <- data.frame(year = i, coef = coef, se = se)
  outdat <- rbind(outdat, indat)
}

ggplot(outdat, aes(year, coef)) + 
  theme_tufte(base_size = 12) +
  geom_line(linetype = "dashed", alpha = 0.3) +
  geom_point() + 
  geom_errorbar(aes(ymin = (coef - 1.96*se), ymax = (coef + 1.96*se)), width = 1.5, alpha = 0.5) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "grey") +
  # ylim(-0.5, 0.01) +
  xlab("Decade") +
  ylab("Degree Day (30C) Coefficient") +
  scale_x_continuous(breaks = c(seq(1910, 2000, 10))) +
  annotate("segment", x=-Inf, xend=Inf, y=-Inf, yend=-Inf, color = "grey") +
  annotate("segment", x=-Inf, xend=-Inf, y=-Inf, yend=Inf, color = "grey")


cropdat <- cropdat %>% 
  group_by(fips) %>% 
  arrange(year) %>% 
  mutate(ln_corn_yield_lag = lag(ln_corn_yield, 1))

thirty <- filter(cropdat, year >= 1930 & year <= 1939)

thirty <- thirty %>% 
  group_by(fips) %>% 
  mutate(corn_yield_avg = mean(corn_yield, na.rm = TRUE)) %>% 
  select(fips, corn_yield_avg)

cropdat <- left_join(cropdat, thirty, by = c("fips"))
cropdat$diff_corn_yield <- cropdat$corn_yield - cropdat$corn_yield_avg

ggplot(filter(cropdat, year >= 1940), aes(year, corn_yield)) + geom_smooth()

fit <- felm(log(1 + corn_yield) ~ dday10_30 + dday30 + prec + prec_sq + 
              dday0_10_rm10 + dday10_30_rm10 + dday30_rm10 + prec_rm10 + prec_sq_rm10 +
              trend + trend_sq | fips | 0 | state, 
           data = cropdat, psdef=FALSE, weights = cropdat$w)
summary(fit)



