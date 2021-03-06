library(tidyverse)
library(ggrepel)
library(ggthemes)

hdat <- read_csv("data/state_hybrid_corn.csv")
hdat <- gather(hdat, key = year, value = value, -state)
hdat$year <- as.numeric(hdat$year)
hdat$db <- ifelse(hdat$state %in% c("Texas", "Oklahoma", "Kansas", "Colorado", "New Mexico", "Nebraska"), 1, 0)
hdat$top <- ifelse(hdat$state %in% c("Iowa", "Indiana", "Illinois"), 1, 0)
hdat0 <- filter(hdat, db == 0)
hdat1 <- filter(hdat, db == 1)
hdat2 <- filter(hdat, top == 1)

ggplot(NULL) + geom_line(data = hdat0, aes(year, value, group = state), color = "grey") +
  geom_line(data = hdat1, aes(year, value, color = state), size = .8, linetype="dotted") +
  geom_line(data = hdat2, aes(year, value, color = state)) +
  geom_label_repel(data = filter(hdat1, year == 1950), aes(year, value, label = state, color = state), segment.color = 'grey50', show.legend = FALSE, nudge_y = 10, nudge_x = -1) +
  geom_label_repel(data = filter(hdat2, year == 1940), aes(year, value, label = state, color = state),  segment.color = 'grey50', show.legend = FALSE, nudge_y = 10, nudge_x = -1) +
  scale_x_continuous(breaks = c(1933, 1935, 1940, 1945, 1950, 1955, 1960)) +
  theme_tufte(base_size = 12) +
  xlab("Year") +
  ylab("Percent of Planted Area with Hybrid Corn") +
  annotate("segment", x=-Inf, xend=Inf, y=-Inf, yend=-Inf, color = "grey") +
  annotate("segment", x=-Inf, xend=-Inf, y=-Inf, yend=Inf, color = "grey") +
  
  theme(legend.position = "none")
  
ggsave("figures/hybrid_corn.pdf", width = 6, height = 4)
