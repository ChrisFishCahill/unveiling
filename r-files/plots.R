library(tidyverse)
data <- readRDS("data/BERTA-wide-0-25.rds")
nrow(data)

nrow(data)
length(unique(data$WBID)) # lake-year surveys

ages <- 2:20
sum(data[, which(colnames(data) %in% ages)]) # how many critters

#----------------------
# study map (Fig 1):
not_surveyed <- read.csv("data/all_lakes.csv")
source("r-files/map.R")

data <- rename(data, "Long_c" = X_long)
data <- rename(data, "Lat_c" = Y_lat)

alberta_map(
  data = data, filename = "plots/Fig_1_map",
  not_surveyed = not_surveyed
)

#----------------------
# Visualizing Paloheimo's effects Fig 2.
sub_dat <- data %>% filter(name == "buck lake")
yr_dat <- sub_dat$year
sub_dat <- sub_dat[, c(which(colnames(sub_dat) %in% 2:25))]
sub_dat$year <- yr_dat
sub_dat <- sub_dat %>%
  pivot_longer(-year, names_to = "age") %>%
  mutate(year = year + 1999)

sub_dat$age <- as.integer(sub_dat$age)
sub_dat$value[sub_dat$value == 0] <- NA
bp <- ggplot(sub_dat, aes(x = year, y = age, size = value)) +
  geom_blank()

bp <- bp + geom_segment(aes(x = 2002.89, xend = 2017, y = 1, yend = 1),
  lineend = "butt", linejoin = "mitre", colour = "steelblue",
  alpha = 0.5, size = 2.5,
  arrow = arrow(length = unit(0.5, "cm"))
)
bp <- bp + geom_segment(aes(x = 2003, xend = 2003, y = 1, yend = 20),
  lineend = "butt", linejoin = "mitre", colour = "steelblue",
  size = 2.5,
  arrow = arrow(length = unit(0.5, "cm"))
)

bp <- bp + geom_segment(aes(x = 2003, xend = 2017, y = 1, yend = 16),
  lineend = "butt", linejoin = "mitre", colour = "steelblue",
  size = 2.75,
  arrow = arrow(length = unit(0.5, "cm"))
)

bp <- bp + geom_point(alpha = 0.75, colour = "black") +
  scale_size(range = c(.5, 5), name = "Number of Fish") +
  ggsidekick::theme_sleek() +
  scale_x_continuous(breaks = seq(2000, 2017, 1)) +
  scale_y_continuous(breaks = c(2, 5, 10, 15, 20), limits = c(1, 20)) +
  expand_limits(x = c(2003, 2017)) +
  ylab("Age") + xlab("Year") +
  theme(
    legend.position = "none",
    axis.title.y = element_text(size = 12),
    axis.title.x = element_text(size = 12),
    axis.text.x = element_text(
      angle = 90, size = 8,
      hjust = 0.95, vjust = 0.2
    ),
    axis.text.y = element_text(size = 8)
    # legend.text = element_text(size = 1),
    # legend.key.size = unit(0.25, 'lines'),
    # legend.direction = "horizontal",
    # axis.text.x = element_text(angle = 90, size=5.5)
  )
bp

bp <- bp + annotate(
  geom = "text", x = 2004, y = 19,
  label = "Age \n effects", col = "black"
)
bp <- bp + annotate(
  geom = "text", x = 2015, y = 17,
  label = "Recruitment \n effects", col = "black"
)
bp <- bp + annotate(
  geom = "text", x = 2015, y = 2.3,
  label = "Year \n effects", col = "black"
)

bp

# ggsave("plots/Fig_2.pdf",
#   width = 6,
#   height = 5
# )
#
# ggsave("plots/Fig_2.png",
#   width = 6,
#   height = 5,
#   dpi = 2000
# )

#----------------------
# Average catch per unit effort creels Fig 3.

data <- read.csv("data/creel.csv")
data <- drop_na(data)
data %>%
  ggplot(aes(x = year, y = cue)) +
  geom_point(size = 2) +
  scale_x_continuous(breaks = seq(1984, 2018, 2)) +
  ggsidekick::theme_sleek() +
  theme(
    legend.position = "none",
    axis.title.y = element_text(size = 12),
    axis.title.x = element_text(size = 12),
    axis.text.x = element_text(
      angle = 90, size = 8,
      hjust = 0.95, vjust = 0.2
    ),
    axis.text.y = element_text(size = 8)
  ) +
  ylab("Catch per Unit Effort (fish/hr)") +
  xlab("Year")

# ggsave("plots/Fig_3.pdf",
#   width = 6,
#   height = 5
# )
#
# ggsave("plots/Fig_3.png",
#   width = 6,
#   height = 5,
#   dpi = 2000
# )

#----------------------
# Equilibrium VPA Fig S1.
v_a <- c(
  0.031538091, 0.267308905,
  0.741673797, 1.115819907,
  1.015923805, 1.293653546,
  0.798531731, 0.623635186,
  0.953930125, 1.345262856,
  1.196369725, 1.131588209,
  0.361576806, 0.886527971,
  1.291708314, 1.101291726
)

U_a <- c(
  0.008591209, 0.07281692,
  0.20203742, 0.303957584,
  0.276745148, 0.352400781,
  0.217525941, 0.169882831,
  0.259857612, 0.366459538,
  0.325899949, 0.308252986,
  0.098496193, 0.241496767,
  0.351870885, 0.33
)

l_a <- c(
  0.368716354, 0.498423931,
  0.601480959, 0.683363231,
  0.748421447, 0.800112386,
  0.841182574, 0.873814218,
  0.899741156, 0.92034098,
  0.936708232, 0.949712563,
  0.960044942, 0.968254364,
  0.974777025, 0.979959499
)

ages <- 2:17
psi <- 2

data <- data.frame(
  age = ages, v_a = v_a, U_a = U_a,
  l_a = l_a, model_v_a = l_a^psi
)
data %>%
  ggplot(aes(x = age, y = v_a)) +
  geom_point(size = 1) +
  geom_line() +
  geom_point(aes(x = age, y = U_a), colour = "steelblue") +
  geom_line(aes(x = age, y = U_a), colour = "steelblue") +
  geom_point(aes(x = age, y = model_v_a), colour = "darkorange2") +
  geom_line(aes(x = age, y = model_v_a), colour = "darkorange2") +
  ylab("Vulnerability and exploitation rate at age") +
  xlab("Age") +
  expand_limits(x = c(2, 20)) +
  scale_x_continuous(breaks = c(2, 5, 10, 15, 20), limits = c(2, 20)) +
  scale_y_continuous(breaks = c(0.0, 0.25, 0.50, 0.75, 1.0, 1.25, 1.5), 
                     limits = c(0, 1.35)) +
  ggsidekick::theme_sleek() +
  theme(
    legend.position = "none",
    axis.title.y = element_text(size = 12),
    axis.title.x = element_text(size = 12),
    axis.text.x = element_text(
      size = 8,
      vjust = 0.2
    ),
    axis.text.y = element_text(size = 8)
  )
#
# ggsave("plots/S1.pdf",
#   width = 6,
#   height = 5
# )
#
# ggsave("plots/S1.png",
#   width = 6,
#   height = 5,
#   dpi = 2000
# )
