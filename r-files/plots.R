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

alberta_map(data = data, filename = "plots/Fig_1_map", 
            not_surveyed = not_surveyed)

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

bp <- bp + annotate(geom = "text", x = 2004, y = 19, 
                    label = "Age \n effects", col = "black")
bp <- bp + annotate(geom = "text", x = 2015, y = 17, 
                    label = "Recruitment \n effects", col = "black")
bp <- bp + annotate(geom = "text", x = 2015, y = 2.3, 
                    label = "Year \n effects", col = "black")

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

unique(data$lake)
data %>%
 ggplot(aes(x=year, y=cue)) + 
  geom_point(size=2) + 
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
