library(tidyverse)
library(tidybayes)
library(rgdal)
#-------------------------
data <- readRDS("data/BERTA-wide-0-25.rds")
stan_files <- list.files(path="D:/unveiling_fits/bev_holt/cr6", pattern = ".rds")
stan_files <- gtools::mixedsort(stan_files) #put .rds files in order

setwd("D:/unveiling_fits/bev_holt/cr6")
hogzilla_list <- 
  stan_files %>%
  purrr::map(function(stan_files){
    readRDS(stan_files)
  })
setwd("C:/Users/Chris_Cahill/Documents/github/unveiling")

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

#----------------------
# Fearly, Flate plot

out <- hogzilla_list %>% map_dfr(function(hogzilla_list){
  hogzilla_list %>%
    spread_draws(v[lake,period]) %>%
    group_by(lake, period) %>%
    mutate(
      Flake = v,
      period = case_when(
        period == 1 ~ "1982-1996",
        period == 2 ~ "1997-2018"
      )
    ) %>%
    summarise(
      lwr = quantile(Flake, 0.1),
      med = quantile(Flake, 0.5),
      upr = quantile(Flake, 0.9),
      lwr2 = quantile(Flake, 0.25),
      upr2 = quantile(Flake, 0.75),
    )
})

#lakes 54/55 are the same...
bad_lakes <- 109:110
out <- out[-bad_lakes,]
#out$name <- unique(data$name)

my_lakes <- rep(1:55, each=2)
my_names <- rep(unique(data$name), each=2)

out$lake <- my_lakes
out$name <- my_names

p <- ggplot(out, aes(y = name, x = med)) +
  geom_linerange(aes(xmin = lwr, xmax = upr), lwd = 0.4) +
  geom_linerange(aes(xmin = lwr2, xmax = upr2), lwd = 0.8) +
  geom_point(aes(colour = period), size = 1.5) +
  xlab("Instantaneous fishing mortality") +
  ylab("") +
  ggsidekick::theme_sleek() +
  theme(axis.text.x = element_text(size = 8)) +
  scale_color_manual(values = c("#b2df8a", "#1f78b4")) +
  scale_x_continuous(breaks = seq(from = -3, to = 3, by = 0.2))

ggsave("plots/fishing_mortality.pdf",
       width = 6,
       height = 6
)

ggsave("plots/fishing_mortality.png",
       width = 6,
       height = 6,
       dpi=2000
)

#----------------------
#----------------------
# multipanel SPR
out <- hogzilla_list %>% map_dfr(function(hogzilla_list){
  hogzilla_list %>%
    spread_draws(SPR[lake])
})

#bookkeeping on lake numbers and names
#Delete second to last lake, this was because ran lakes in 2's 
#(i.e., lake 54/55 are the same)
ndraw <- length(unique(out$.draw))

#Get the first and last draw of hogwash lake
bad_lake_index <- (nrow(out)-ndraw-ndraw+1):(nrow(out)-ndraw)

out <- out[-bad_lake_index,]

my_lakes <- rep(1:55, each=ndraw)
my_names <- rep(unique(data$name), each=ndraw)

out$lake <- my_lakes
out$name <- my_names

out <- out %>% 
  mutate(pr_sum = ifelse(SPR < 0.4, 1,0)) %>%
  group_by(name) %>%
  mutate(pr = format(round(sum(pr_sum) / ndraw,2), nsmall=2) )

out$name2 <- paste("Pr(<0.4):", out$pr, sep=" ")
out$name2 <- paste(out$name, out$name2, sep=" ")
out$name2

p <- out %>%
  ggplot(aes(x=SPR)) + 
  geom_histogram(binwidth = 0.05, fill = "grey", color = "black") + 
  scale_x_continuous(breaks = c(0.0, 0.25, 0.50, 0.75, 1.00), 
                     limits=c(0,1.05)) +
  facet_wrap(~name2, 
             scales = "free_y", 
             labeller = label_wrap_gen(width = 18, multi_line=TRUE))+
  xlab("Spawner potential ratio") + 
  ylab("Count") + 
  geom_vline(xintercept = 0.4, lty = 1, size = 1, alpha = 0.85, 
             colour="steelblue") +
  #geom_text(data=labels, aes(label=pr), x = 0.75, y = Inf, hjust = 0, vjust = 1, 
  #          inherit.aes = FALSE, colour="black", size=3) +
  ggsidekick::theme_sleek()+
  theme(
    legend.position="none", 
    axis.line = element_line(colour = "grey30"),
    axis.title = element_text(size = 12, colour = "grey30"),
    strip.text.x = element_text(size=8, colour = "grey30"),
    panel.spacing.x = unit(1, "lines"),
    panel.spacing.y = unit(0.5, "lines"),
    panel.border = element_blank(),
    axis.text.x = element_text(angle=90, size=10),
    axis.text.y = element_text(size=8)
  ) 
p 
ggsave("plots/SPR.pdf",
       width = 11,
       height = 8
)

ggsave("plots/SPR.png",
       width = 11,
       height = 8, 
       dpi=2000
)

#----------------------
# Province-wide Kobe plot with Flate

out <- hogzilla_list %>% map_dfr(function(hogzilla_list){
  hogzilla_list %>%
    spread_draws(b_ratio[lake], F_ratio[lake]) %>%
    median_qi() %>%
    mutate(
      value = b_ratio,
      value = F_ratio
    )
})

#bookkeeping on lake numbers and names
out <- out[-55,]
out$lake <- 1:55
out$name <- unique(data$name)

data2 <- data %>% dplyr::select(name, X_long, Y_lat)
data2 <- unique(data2)

out <- left_join(out, data2, by="name")

kp <- out %>%
ggplot(aes(x = b_ratio, y = F_ratio)) +
  annotate("rect", xmin=-Inf, xmax=0.4, ymin=1.0, ymax=Inf,
           alpha = .75, fill="firebrick") +
  annotate("rect", xmin=-Inf, xmax=0.4, ymin=-Inf, ymax=1.0,
           alpha = .75, fill="goldenrod2") +
  annotate("rect", xmin=0.4, xmax=Inf, ymin=-Inf, ymax=1.0,
           alpha = .75, fill="forestgreen") +  
  annotate("rect", xmin=0.4, xmax=Inf, ymin=1.0, ymax=Inf,
           alpha = .75, fill="goldenrod2") + 
  geom_point(pch = 21, fill="black", size = 1.0) +
  #geom_text(aes(label=name),hjust=0, vjust=0) + 
  ggsidekick::theme_sleek() +
  ylab(expression(F[late]/F[msy])) +
  xlab("Mean survey SSB / SSBo") +
  scale_x_continuous(breaks = seq(0, 3, .5)) +
  geom_hline(yintercept = 1, lty = 1, size = 0.75, alpha = 0.25) +
  geom_vline(xintercept = 0.4, lty = 1, size = 0.75, alpha = 0.25) +
  theme(
    axis.text.x = element_text(size = 10),
    axis.text.y = element_text(size = 10),
    axis.title = element_text(size = 12, colour = "grey30"),
  )

kp <- kp + ggrepel::geom_label_repel(aes(label = name),
               segment.color = "grey50",
              label.size=NA, size=1.75, box.padding = 0.25,
              min.segment.length = 0, 
              fill = alpha(c("white"),0)) 
kp 
# 
# ggsave("plots/alta_Kobe_v2.png",
#        width = 8,
#        height = 6,
#        dpi = 2000
# )
# 
# ggsave("plots/alta_Kobe_v2.pdf",
#        width = 8,
#        height = 8
# )

#Can we make a spatial Kobe plot? 
can1 <- raster::getData("GADM", country = "CAN", level = 1)
alta <- can1[can1$NAME_1 %in% "Alberta", ]

alta <- spTransform(
  alta,
  CRS("+proj=longlat +datum=WGS84")
)
alta.fort <- fortify(alta)

names(alta.fort)[1] <- "X_long"
names(alta.fort)[2] <- "Y_lat"

out <- out %>% mutate(colour = ifelse(b_ratio <= 0.4 & F_ratio <= 1.0, "goldenrod2", 
                               ifelse(b_ratio >= 0.4 & F_ratio >= 1.0, "goldenrod2",
                               ifelse(b_ratio <= 0.4 & F_ratio >= 1.0, "firebrick",
                                      "forestgreen"))))

kobe_map <- ggplot(NULL) +
  geom_polygon(colour = "black", fill = "white", 
               data = alta.fort, aes(x = X_long, y = Y_lat, group = id)) +
  geom_point(pch = 21, size = 1.5, data = out, aes(X_long, Y_lat, fill=colour)) +
  scale_x_continuous(breaks = c(-120, -115, -110)) +
  scale_y_continuous(breaks = c(49, 52, 56, 60)) +
  scale_fill_manual(values=alpha(c("firebrick", "forestgreen", "goldenrod2"),0.75))

kobe_map <- kobe_map + 
  ylab("Latitude") + xlab("Longitude")

kobe_map <- kobe_map + ggalt::coord_proj(
  paste0(CRS("+proj=tmerc +lat_0=0 +lon_0=-115 +k=0.9992 +x_0=500000 +y_0=0 +ellps=GRS80 +datum=NAD83 +units=m +no_defs"))
)

kobe_map <- kobe_map + ggsidekick::theme_sleek() +
  theme(
    legend.position="none", 
    axis.line = element_line(colour = "grey30"),
    axis.title = element_text(size = 12, colour = "grey30"),
    strip.background = element_blank(),
    strip.text.x = element_blank(),
    panel.spacing.x = unit(1, "lines"),
    panel.spacing.y = unit(0.5, "lines"),
    panel.border = element_blank()
  )
kobe_map

my_map <- kp + annotation_custom(
  ggplotGrob(kobe_map), 
  xmin = 1.78, xmax = Inf, ymin = 1.92, ymax = Inf
 )

# ggsave("plots/spatial_kobe.pdf",
#         width = 8,
#         height = 8
# )
# 
# ggsave("plots/spatial_kobe.png",
#        width = 8,
#        height = 8,
#        dpi=2000
# )

#------------------------------------
# Bubble plots for Pigeon, LLB, Winefred, Lac Ste. Anne
my_names <- c("pigeon lake", "lac la biche", "winefred lake", "lesser slave lake", 
              "amisk lake", "lac ste. anne", "gods lake", 
              "baptiste lake", "lake newell")
sub_dat <- data %>% filter(name %in% my_names)
yr_dat <- sub_dat$year
name_dat <- sub_dat$name
sub_dat <- sub_dat[, c(which(colnames(sub_dat) %in% 2:25))]
sub_dat$year <- yr_dat
sub_dat$name <- name_dat
drop_cols <- c("year", "name")
sub_dat <- sub_dat %>%
  pivot_longer(-one_of(drop_cols), names_to = c("age")) %>%
  mutate(year = year + 1999)

sub_dat$age <- as.integer(sub_dat$age)
sub_dat$value[sub_dat$value == 0] <- NA
bp <- ggplot(sub_dat, aes(x = year, y = age, size = value)) +
  geom_blank() +
  facet_wrap(~name)

bp <- bp + geom_point(alpha = 0.75, colour = "black") +
  scale_size(range = c(.5, 5), name = "Number of Fish") +
  ggsidekick::theme_sleek() +
  scale_x_continuous(breaks = seq(2000, 2018, 1)) +
  scale_y_continuous(breaks = c(2, 5, 10, 15, 20), limits = c(1, 20)) +
  expand_limits(x = c(2000, 2018)) +
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

#bunch of bookkeeping to add in the mortality rates:
# out <- hogzilla_list %>% map_dfr(function(hogzilla_list){
#   hogzilla_list %>%
#     spread_draws(v[lake,period]) %>%
#     group_by(lake, period) %>%
#     mutate(
#       Flake = v,
#       period = case_when(
#         period == 1 ~ "1982-1996",
#         period == 2 ~ "1997-2018"
#       )
#     ) %>%
#     summarise(
#       lwr = quantile(Flake, 0.1),
#       med = quantile(Flake, 0.5),
#       upr = quantile(Flake, 0.9),
#       lwr2 = quantile(Flake, 0.25),
#       upr2 = quantile(Flake, 0.75),
#     )
# })
# 
# #lakes 54/55 are the same...
# bad_lakes <- 109:110
# out <- out[-bad_lakes,]
# #out$name <- unique(data$name)
# 
# my_lakes <- rep(1:55, each=2)
# my_names <- rep(unique(data$name), each=2)
# 
# out$lake <- my_lakes
# out$name <- my_names

my_dat <- out %>% filter(name %in% my_names, 
                         period=="1982-1996")

my_dat$med <- format(round(my_dat$med, 2), nsmall=2)
my_dat$med <- paste0("'", my_dat$med, "'")
my_dat$med <- paste("F[early]:", my_dat$med, sep=" ")

my_dat2 <- out %>% filter(name %in% my_names, 
                         period=="1997-2018")
my_dat2$med <- format(round(my_dat2$med, 2), nsmall=2)
my_dat2$med <- paste0("'", my_dat2$med, "'")
my_dat2$med <- paste("F[late]:", my_dat2$med, sep=" ")

bp <- bp + geom_text(data=my_dat, aes(label=med),
                     x = 2001.5, y = 19, 
                     colour="black", size=2.5, parse=T)

bp <- bp + geom_text(data=my_dat2, aes(label=med),
                     x = 2001.5, y = 17, 
                     colour="black", size=2.5, parse=T)
bp 

ggsave("plots/bubble.pdf",
  width = 8.5,
  height = 5
)

ggsave("plots/bubble.png",
  width = 8,
  height = 6,
  dpi = 2000
)

#------------------------------------
#Let's try and plot this emergent recruitment pulse in 2000
# This is some filthy trickery to manipulate massive stan fits
out <- hogzilla_list %>% map_dfr(function(hogzilla_list){
  hogzilla_list %>%
    spread_draws(R2[lake, year]) %>%
    median_qi() %>%
    mutate(
      value = R2, 
      year = year + 1979
    )
})

#get rid of second to last lake which was fit twice:
out <- out[-c(2647:2695),]

my_lakes <- rep(1:55, each=length(1980:2028))
my_names <- rep(unique(data$name), each=length(1980:2028))

out$lake <- my_lakes
out$name <- my_names

out <- out %>% 
  group_by(name) %>%
  mutate(mean_R2 = mean(R2))
out$mean_std_R2 = out$R2 - out$mean_R2

which_lakes <- c("amisk lake", "crawling valley reservoir", 
                 "kinnaird lake", "shiningbank lake", 
                 "unnamed 4", "baptiste lake", "elinor lake", 
                 "moose lake", "skeleton lake", "hilda lake", 
                 "smoke lake", "whitefish lake", "lac la nonne", 
                 "iosegun lake", "fickle lake", "blackett lake", 
                 "lac ste. anne", "pigeon lake", "jackson lake", 
                 "garner lake", "buck lake", "calling lake", 
                 "gods lake", "kehiwin lake", "lesser slave lake", 
                 "rock island lake", "sylvan lake", 
                 "wolf lake 2", "touchwood lake", "long lake 1", 
                 "christina lake", "unnamed 5")

out$pulse <- ifelse(out$name %in% which_lakes, "yes", "no")

p <- out %>%
  ggplot(aes(x=year, y=mean_std_R2)) +
  geom_rect(data = data.frame(name = which_lakes), 
            aes(xmin = 1997, xmax = 2003, ymin = -Inf, ymax = Inf), 
            alpha = 0.5, fill="steelblue", inherit.aes = FALSE) + 
  geom_point(size=0.75) +
  geom_line() + 
  scale_x_continuous(breaks = c(1980, 1990, 2000, 2010, 2018), 
                     limits=c(1980,2018)) +
  facet_wrap(~name, scales = "free_y", 
             labeller = label_wrap_gen()) + 
  ylab("Age 2 Walleye (mean centered)") + 
  xlab("Year") + 
  theme(
    legend.position="none", 
    axis.line = element_line(colour = "grey30"),
    axis.title = element_text(size = 12, colour = "grey30"),
    strip.text.x = element_text(size=8, colour = "grey30"),
    panel.spacing.x = unit(1, "lines"),
    panel.spacing.y = unit(0.5, "lines"),
    panel.border = element_blank(),
    axis.text.x = element_text(angle = 90, size=10),
    axis.text.y = element_text(size=10)
  )

ggsave("plots/centered_R2_all_lakes.pdf",
       width = 11,
       height = 8
)

ggsave("plots/centered_R2_all_lakes.png",
       width = 11,
       height = 8, 
       dpi=2000
)

p <- out %>%
  filter(name %in% which_lakes) %>%
  ggplot(aes(x=year, y=mean_std_R2)) +
  geom_rect(data = data.frame(name = which_lakes), 
            aes(xmin = 1998, xmax = 2002, ymin = -Inf, ymax = Inf), 
            alpha = 0.5, fill="steelblue", inherit.aes = FALSE) + 
  geom_point() +
  geom_line() + 
  scale_x_continuous(breaks = c(1980, 1990, 2000, 2010, 2018), 
                     limits=c(1980,2018)) +
  facet_wrap(~name, scales = "free_y", 
             nrow=8,
             labeller = label_wrap_gen()) + 
  ylab("Age 2 Walleye (mean centered)") + 
  xlab("Year") + 
  theme(
    legend.position="none", 
    axis.line = element_line(colour = "grey30"),
    axis.title = element_text(size = 12, colour = "grey30"),
    strip.text.x = element_text(size=8, colour = "grey30"),
    panel.spacing.x = unit(1, "lines"),
    panel.spacing.y = unit(0.5, "lines"),
    panel.border = element_blank()
  )
p

out <- left_join(out, data2, by="name")

pulse_map <- ggplot(NULL) +
  geom_polygon(colour = "black", fill = "white", 
               data = alta.fort, aes(x = X_long, y = Y_lat, group = id)) +
  geom_point(pch = 21, size = 1.5, data = out, aes(X_long, Y_lat, fill=pulse)) +
  scale_x_continuous(breaks = c(-120, -115, -110)) +
  scale_y_continuous(breaks = c(49, 52, 56, 60)) + 
  scale_fill_manual(values=alpha(c("white", "steelblue"), 0.75))

pulse_map <- pulse_map + 
  ylab("Latitude") + xlab("Longitude")

pulse_map <- pulse_map + ggalt::coord_proj(
  paste0(CRS("+proj=tmerc +lat_0=0 +lon_0=-115 +k=0.9992 +x_0=500000 +y_0=0 +ellps=GRS80 +datum=NAD83 +units=m +no_defs"))
)

pulse_map <- pulse_map + ggsidekick::theme_sleek() +
  theme(
    legend.position="none", 
    axis.line = element_line(colour = "grey30"),
    axis.title = element_text(size = 12, colour = "grey30"),
    strip.background = element_blank(),
    strip.text.x = element_blank(),
    panel.spacing.x = unit(1, "lines"),
    panel.spacing.y = unit(0.5, "lines"),
    panel.border = element_blank()
  )
pulse_map

ggsave("plots/pulse_map.pdf",
       width = 3,
       height = 5
)

my_map <- cowplot::plot_grid(p, pulse_map,
                             nrow = 1, 
                             rel_widths = c(0.66,0.33)
)

ggsave("plots/pulse_map.pdf",
       width = 15,
       height = 8
)

ggsave("plots/pulse_map.png",
       width = 15,
       height = 8, 
       dpi=2000
)

#------------------------------------
#Multipanel time trajectory plot

r2 <- hogzilla_list %>% map_dfr(function(hogzilla_list){
  hogzilla_list %>%
    spread_draws(R2[lake, year]) %>%
    mutate(
      value = R2, 
      year = year + 1979
    ) %>%
    summarise(
      lwr = quantile(R2, 0.1),
      med = quantile(R2, 0.5),
      upr = quantile(R2, 0.9),
      lwr2 = quantile(R2, 0.25),
      upr2 = quantile(R2, 0.75),
    )
})

ssb <- hogzilla_list %>% map_dfr(function(hogzilla_list){
  hogzilla_list %>%
    spread_draws(SSB[lake, year]) %>%
    mutate(
      value = SSB, 
      year = year + 1979
    ) %>%
    summarise(
      lwr = quantile(SSB, 0.1),
      med = quantile(SSB, 0.5),
      upr = quantile(SSB, 0.9),
      lwr2 = quantile(SSB, 0.25),
      upr2 = quantile(SSB, 0.75),
    )
})

ssb_c <- hogzilla_list %>% map_dfr(function(hogzilla_list){
  hogzilla_list %>%
    spread_draws(SSB_obs[Nobs]) %>%
    median_qi() %>%
    mutate(
      SSB_obs = SSB_obs,
      SSB_lower = SSB_obs - 0.30 * SSB_obs,
      SSB_upper = SSB_obs + 0.30 * SSB_obs
    )
})


#get rid of second to last lake which was fit twice:
r2 <- r2[-c(2647:2695),]
ssb <- ssb[-c(2647:2695),]

my_lakes <- rep(1:55, each=length(1980:2028))
my_names <- rep(unique(data$name), each=length(1980:2028))

r2$lake <- my_lakes
r2$name <- my_names

ssb$lake <- my_lakes
ssb$name <- my_names

ssb$which <-"female \nbiomass"
r2$which <- "recruits"

#deal with ssb_c
bogus_fit <- 234:236
ssb_c = ssb_c[-bogus_fit,]
ssb_c$year <- data$year+1999
ssb_c$name <- data$name

trajectory_dat <- rbind(ssb, r2)

trajectory_dat <- left_join(trajectory_dat, ssb_c,
                            by = c("name", "year"))


which_lakes <- c("pigeon lake", "lac la biche", "winefred lake", "lesser slave lake", 
              "amisk lake", "lac ste. anne", "gods lake", 
              "baptiste lake", "lake newell")


trajectory_plot <- trajectory_dat %>%
  filter(name %in% which_lakes) %>%
  ggplot(aes(x = year, y = med, colour = factor(which), fill = factor(which) )) +
  geom_line(lwd = 0.5) +
  geom_ribbon(aes(ymin = lwr2, ymax = upr2),
              linetype = 0, 
              alpha = 0.5 
  ) +
  geom_ribbon(aes(ymin = lwr, ymax = upr),
              linetype = 2, lwd = 0.5,
              alpha = 0.25 
  ) +
  scale_fill_manual(values = c("steelblue", "darkorange2"), name="") +  
  scale_color_manual(values = c("steelblue", "darkorange2"), name="") +
  xlab("Year") +
  ylab("Female biomass (kg/ha)") +
  xlim(1980, 2028) +
  scale_x_continuous(breaks = c(1980, 1990, 2000, 2010, 2020, 2028)) +
  ggsidekick::theme_sleek() +
  geom_point(aes(x = year, y = SSB_obs), size=0.5, colour = "black", 
             show_guide = FALSE) +
  geom_linerange(aes(x = year, ymin = SSB_lower, ymax = SSB_upper),
                 colour = "black") +
  # geom_point(aes(x = year + 2, y = stock),
  #            shape = 23, fill = "darkorange2",
  #            color = "steelblue", size = 3
  # ) +
  scale_y_continuous(sec.axis = sec_axis(~ . * 1,
                                         name = "Age 2 recruits (N/ha)"
  )) +
  theme(
    #legend.position = "none",
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
  ) + 
  facet_wrap(~name, ncol=3, scales = "free") 

trajectory_plot 


ggsave("plots/trajectory.pdf",
       width = 8,
       height = 5
)

ggsave("plots/trajectory.png",
       width = 8,
       height = 5, 
       dpi=2000
)

pdf("plots/S3.pdf",
    width = 8, height = 11
)

#Now make S3 for all lakes
for(i in 1:3){
trajectory_plot <- trajectory_dat %>%
  filter(!(name %in% which_lakes)) %>%
  ggplot(aes(x = year, y = med, colour = factor(which), fill = factor(which) )) +
  geom_line(lwd = 0.5) +
  geom_ribbon(aes(ymin = lwr2, ymax = upr2),
              linetype = 0, 
              alpha = 0.5 
  ) +
  geom_ribbon(aes(ymin = lwr, ymax = upr),
              linetype = 2, lwd = 0.5,
              alpha = 0.25 
  ) +
  scale_fill_manual(values = c("steelblue", "darkorange2"), name="") +  
  scale_color_manual(values = c("steelblue", "darkorange2"), name="") +
  xlab("Year") +
  ylab("Female biomass (kg/ha)") +
  xlim(1980, 2028) +
  scale_x_continuous(breaks = c(1980, 1990, 2000, 2010, 2020, 2028)) +
  ggsidekick::theme_sleek() +
  geom_point(aes(x = year, y = SSB_obs), size=0.5, colour = "black", 
             show_guide = FALSE) +
  geom_linerange(aes(x = year, ymin = SSB_lower, ymax = SSB_upper),
                 colour = "black") +
  # geom_point(aes(x = year + 2, y = stock),
  #            shape = 23, fill = "darkorange2",
  #            color = "steelblue", size = 3
  # ) +
  scale_y_continuous(sec.axis = sec_axis(~ . * 1,
                                         name = "Age 2 recruits (N/ha)"
  )) +
  theme(
    #legend.position = "none",
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
  ) + 
  ggforce::facet_wrap_paginate(~name, ncol=3, nrow=6, scales = "free", page=i) 
print(trajectory_plot)
}
dev.off()

#------------------------------------
#stocking
stocking <- readRDS("data/stocking_matrix_ha.rds")
which_lakes <- c("pigeon lake", "buck lake", "lac la biche", "lake newell")
stocking <- stocking[which(rownames(stocking) %in% which_lakes),]
colnames(stocking) <- 1980:2018
stocking <- as.data.frame(stocking)
stocking$name <- rownames(stocking)

stock_surv <- 0.01
stocking <- stocking %>% 
  pivot_longer(cols = !name, names_to = "year", 
               names_transform = list(year = as.numeric),
               values_to="stock") %>%
  mutate(stock = stock*stock_surv) %>%
  mutate(stock = ifelse(stock==0, NA, stock))

stocking
sum(!is.na(stocking$stock)) #147 events
range(stocking$stock, na.rm =T) #0.01 to 40.8 fish per ha
mean(stocking$stock, na.rm =T) #4.6 fish 

r2 <- hogzilla_list %>% map_dfr(function(hogzilla_list){
  hogzilla_list %>%
    spread_draws(R2[lake, year]) %>%
    mutate(
      value = R2, 
      year = year + 1979
    ) %>%
    summarise(
      lwr = quantile(R2, 0.1),
      med = quantile(R2, 0.5),
      upr = quantile(R2, 0.9),
      lwr2 = quantile(R2, 0.25),
      upr2 = quantile(R2, 0.75),
    )
})

ssb <- hogzilla_list %>% map_dfr(function(hogzilla_list){
  hogzilla_list %>%
    spread_draws(SSB[lake, year]) %>%
    mutate(
      value = SSB, 
      year = year + 1979
    ) %>%
    summarise(
      lwr = quantile(SSB, 0.1),
      med = quantile(SSB, 0.5),
      upr = quantile(SSB, 0.9),
      lwr2 = quantile(SSB, 0.25),
      upr2 = quantile(SSB, 0.75),
    )
})

ssb_c <- hogzilla_list %>% map_dfr(function(hogzilla_list){
  hogzilla_list %>%
    spread_draws(SSB_obs[Nobs]) %>%
    median_qi() %>%
    mutate(
      SSB_obs = SSB_obs,
      SSB_lower = SSB_obs - 0.30 * SSB_obs,
      SSB_upper = SSB_obs + 0.30 * SSB_obs
    )
})


#get rid of second to last lake which was fit twice:
r2 <- r2[-c(2647:2695),]
ssb <- ssb[-c(2647:2695),]

my_lakes <- rep(1:55, each=length(1980:2028))
my_names <- rep(unique(data$name), each=length(1980:2028))

r2$lake <- my_lakes
r2$name <- my_names

ssb$lake <- my_lakes
ssb$name <- my_names

ssb$which <-"female \nbiomass"
r2$which <- "recruits"

#deal with ssb_c
bogus_fit <- 234:236
ssb_c = ssb_c[-bogus_fit,]
ssb_c$year <- data$year+1999
ssb_c$name <- data$name

trajectory_dat <- r2 %>% filter(year <= 2018)

trajectory_dat <- left_join(trajectory_dat, ssb_c,
                            by = c("name", "year"))

trajectory_dat <- left_join(trajectory_dat, stocking, 
                            by = c("name", "year"))
trajectory_dat$name <- factor(trajectory_dat$name,      # Reordering group factor levels
                         levels = c("lac la biche", "pigeon lake", "lake newell", 
                                    "buck lake"))

trajectory_plot <- trajectory_dat %>%
  filter(name %in% which_lakes) %>%
  ggplot(aes(x = year, y = med, colour = factor(which), fill = factor(which) )) +
  geom_line(lwd=0.75) +
  #geom_point() + 
  # geom_ribbon(aes(ymin = lwr2, ymax = upr2),
  #             linetype = 0, 
  #             alpha = 0.5 
  # ) +
  # geom_ribbon(aes(ymin = lwr, ymax = upr),
  #             linetype = 2, lwd = 0.5,
  #             alpha = 0.25 
  # ) +
  scale_fill_manual(values = c("black"), name="") +  
  scale_color_manual(values = c("black"), name="") +
  xlab("Year") +
  ylab("Age 2 recruits (N/ha)") +
  geom_point(aes(x = year + 2, y = stock),
             shape = 23, fill = "darkorange2",
             color = "darkorange2", size = 0.75
  ) +
  xlim(1980, 2017) +
  scale_x_continuous(breaks = c(1980, 1990, 2000, 2010, 2018)) +
  ggsidekick::theme_sleek() +
  # scale_y_continuous(sec.axis = sec_axis(~ . * 1,
  #                                        name = "Age 2 Recruits (N/ha)"
  # )) +
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
  ) + 
  facet_wrap(~name, ncol=2, scales = "free") 

trajectory_plot 


ggsave("plots/stocking.pdf",
       width = 3.75,
       height = 3.75, 
)

ggsave("plots/stocking.png",
       width = 5,
       height = 4, 
       dpi=2000
)
#------------------------------------
#------------------------------------
#egg sensitivity plot
#data <- readRDS("data/BERTA-wide-0-25.rds")
stan_files <- list.files(path="D:/unveiling_fits/bev_holt/cr6", pattern = ".rds")
stan_files <- gtools::mixedsort(stan_files) #put .rds files in order

setwd("D:/unveiling_fits/bev_holt/cr6")
hogzilla_list <- 
  stan_files %>%
  purrr::map(function(stan_files){
    readRDS(stan_files)
  })

setwd("D:/unveiling_fits/bev_holt/cr12")
stan_files <- list.files(path="D:/unveiling_fits/bev_holt/cr12", pattern = ".rds")
stan_files <- gtools::mixedsort(stan_files) 

hogzilla_list2 <- 
  stan_files %>%
  purrr::map(function(stan_files){
    readRDS(stan_files)
  })

setwd("D:/unveiling_fits/ricker/cr6")
stan_files <- list.files(path="D:/unveiling_fits/ricker/cr6", pattern = ".rds")
stan_files <- gtools::mixedsort(stan_files) 

hogzilla_list3 <- 
  stan_files %>%
  purrr::map(function(stan_files){
    readRDS(stan_files)
  })

setwd("D:/unveiling_fits/ricker/cr12")
stan_files <- list.files(path="D:/unveiling_fits/ricker/cr12", pattern = ".rds")
stan_files <- gtools::mixedsort(stan_files) 

hogzilla_list4 <- 
  stan_files %>%
  purrr::map(function(stan_files){
    readRDS(stan_files)
  })

setwd("C:/Users/Chris_Cahill/Documents/github/unveiling")

bh6 <- hogzilla_list %>% map_dfr(function(hogzilla_list){
  hogzilla_list %>%
    spread_draws(b_ratio[lake], F_ratio[lake])
})

bh12 <- hogzilla_list2 %>% map_dfr(function(hogzilla_list2){
  hogzilla_list2 %>%
    spread_draws(b_ratio[lake], F_ratio[lake])
})

ricker6 <- hogzilla_list3 %>% map_dfr(function(hogzilla_list3){
  hogzilla_list3 %>%
    spread_draws(b_ratio[lake], F_ratio[lake])
})

ricker12 <- hogzilla_list4 %>% map_dfr(function(hogzilla_list4){
  hogzilla_list4 %>%
    spread_draws(b_ratio[lake], F_ratio[lake])
})

#bookkeeping on lake numbers and names
#Delete second to last lake, this was because ran lakes in 2's 
#(i.e., lake 54/55 are the same)
ndraw <- length(unique(bh6$.draw))

#Get the first and last draw of hogwash lake
bad_lake_index <- (nrow(bh6)-ndraw-ndraw+1):(nrow(bh6)-ndraw)

bh6 <- bh6[-bad_lake_index,]
bh12 <- bh12[-bad_lake_index,]
ricker6 <- ricker6[-bad_lake_index,]
ricker12 <- ricker12[-bad_lake_index,]

my_lakes <- rep(1:55, each=ndraw)
my_names <- rep(unique(data$name), each=ndraw)

bh6$lake <- bh12$lake <- ricker6$lake <- ricker12$lake <- my_lakes
bh6$name <- bh12$name <- ricker6$name <- ricker12$name <- my_names

bh6$label_parsed <- "Beverton-Holt~Phi==6"
bh12$label_parsed <- "Beverton-Holt~Phi==12"
ricker6$label_parsed <- "Ricker~Phi==6"
ricker12$label_parsed <- "Ricker~Phi==12"
bh6$level <- factor("bh6")
bh12$level <- factor("bh12")
ricker6$level <- factor("ricker6")
ricker12$level <- factor("ricker12")

#Egg function
getLevel <- function(x, y, prob = 0.95) {
  kk <- MASS::kde2d(x, y)
  dx <- diff(kk$x[1:2])
  dy <- diff(kk$y[1:2])
  sz <- sort(kk$z)
  c1 <- cumsum(sz) * dx * dy
  approx(c1, sz, xout = 1 - prob)$y
}

all_runs <- rbind(bh6, bh12, ricker6, ricker12)

# egglette
lake_name = c("pigeon lake", "lac ste. anne", "lac la biche", "lake newell")
my_dat <- all_runs %>%
  filter(name == lake_name) %>%
  group_by(name, level) %>%
  mutate(L90 = getLevel(b_ratio, F_ratio, prob = 0.90), 
         L75 = getLevel(b_ratio, F_ratio, prob=0.75), 
         L50 = getLevel(b_ratio, F_ratio, prob=0.5) ) %>%
  mutate(name = str_replace_all(name, " ", "~"))

my_dat$name <- factor(my_dat$name,  # Reordering group factor levels
                      levels = c("pigeon~lake", "lac~ste.~anne", 
                                 "lac~la~biche", "lake~newell")
)

my_dat$label_parsed <- factor(my_dat$label_parsed,  # Reordering group factor levels
                      levels = c("Beverton-Holt~Phi==6",  
                                 "Ricker~Phi==6", 
                                 "Beverton-Holt~Phi==12",
                                 "Ricker~Phi==12")
)


egglette <- my_dat %>%
  ggplot(aes(x = b_ratio, y = F_ratio)) +
  #xlim(0, 5.0) +
  #ylim(0, ymax) +
  # stat_density2d(colour="darkorange2",breaks=L75, size=1.2)+

  stat_density2d(
    colour = "darkorange2", geom = "polygon", breaks = L50,
    fill = "darkorange2", size = 0.75
  ) +
  stat_density2d(
    colour = "gray", contour = T, geom = "polygon",
    fill = "snow", alpha = 0, breaks = L90, size = 0.75
  )  + 
  geom_point(pch = 16, size = 0.01, alpha = 0.15) +
  ggsidekick::theme_sleek() +
  ylab(expression(F[late]/F[msy])) +
  xlab("Mean survey SSB / SSBo") +
  #scale_x_continuous(breaks = seq(0, 5, .5)) +
  geom_hline(yintercept = 1, lty = 1, size = 0.75, alpha = 0.25) +
  geom_vline(xintercept = 0.4, lty = 1, size = 0.75, alpha = 0.25) +
  theme(
    axis.text.x = element_text(size = 8),
    axis.text.y = element_text(size = 8),
    axis.title = element_text(size = 12, colour = "grey30"),
    panel.spacing = unit(1.0, "lines")
  ) +
  facet_grid(vars(name), vars(label_parsed), 
             scales = "free", labeller = label_parsed)

egglette 

ggsave("plots/egg_sensitivity.png",
       width = 8,
       height = 6,
       dpi = 2000
)

ggsave("plots/egg_sensitivity.pdf",
       width = 8,
       height = 6.25
)


#------------------------------------
#------------------------------------
#Plot compensation ratio vs. GDD
# Figure 4: Maps of R0, Flate, Fmsy, 
# This is some filthy trickery to manipulate massive stan fits
out <- hogzilla_list %>% map_dfr(function(hogzilla_list){
  hogzilla_list %>%
    spread_draws(ar[lake], sbr0_kick[lake], 
                 R0[lake]) %>%
    mutate(
      cr = exp(ar)*sbr0_kick,
      SSBo = sbr0_kick*R0
    ) %>%
  median_qi()
})

data <- read.csv("data/BERTA-wide-0-25.csv")

#bookkeeping on lake numbers and names
#Delete second to last lake, this was because ran lakes in 2's 
#(i.e., lake in rows 54/55 are the same)
out <- out[-55,]

out$name <- unique(data$name)

data2 <- data %>% dplyr::select(name, X_long, Y_lat, Area_Ha, DD5, Mean_Depth, 
                                X_TTM_c, Y_TTM_c, Basin_ha, TS, TP, Chloro, TDS)
data2 <- unique(data2)

out <- left_join(out, data2, by="name")

p <- out %>%
  ggplot(aes(x=DD5, y=cr))+ 
  geom_point(size=1.0) + 
  geom_linerange(aes(x = DD5, ymin = cr.lower, ymax = cr.upper),
                 colour = "black") +
  xlab("Growing Degree Days > 5 (air proxy)") + 
  ylab(expression(Compensation~ratio~phi)) + 
  ggsidekick::theme_sleek()+
  theme(
    axis.text.x = element_text(size = 8),
    axis.text.y = element_text(size = 8),
    axis.title = element_text(size = 12, colour = "grey30"),
  ) + 
  stat_smooth()

ggsave("plots/cr_v_gdd.pdf",
       width = 6,
       height = 4
)

ggsave("plots/cr_v_gdd.pdf.png",
       width = 6,
       height = 4,
       dpi = 2000
)

#################################################
#S4 figure of plots vs. h20 stuff

out <- out %>% mutate(TS = recode(TS, 
                           eutrophic = 'eu', 
                           mesotrophic = "meso", 
                           oligotrophic = "olig", 
                           'hyper-eutrophic' = "hyper"
)               )

unique(out$TS)

out$TS <- factor(out$TS, 
                 levels = c("hyper", "eu", "meso", "olig"))

p <- out %>%
  ggplot(aes(x=TP, y=SSBo))+ 
  geom_point(size=0.5) + 
  geom_linerange(aes(x = TP, ymin = SSBo.lower, ymax = SSBo.upper),
                 colour = "black") +
  xlab("Total Phosphorous (mg/l)") + 
  ylab("SSBo (kg/ha)") + 
  ggsidekick::theme_sleek()+
  theme(
    axis.text.x = element_text(size = 8),
    axis.text.y = element_text(size = 8),
    axis.title = element_text(size = 10, colour = "grey30")
  )  

p 

p1 <- out %>%
  ggplot(aes(x=Chloro, y=SSBo))+ 
  geom_point(size=0.5) + 
  geom_linerange(aes(x = Chloro, ymin = SSBo.lower, ymax = SSBo.upper),
                 colour = "black") +
  xlab("Chlorophyll a") + 
  ylab("SSBo (kg/ha)") + 
  ggsidekick::theme_sleek()+
  theme(
    axis.text.x = element_text(size = 8),
    axis.text.y = element_text(size = 8),
    axis.title = element_text(size = 10, colour = "grey30")
  )  

p1 

p2 <- out %>%
  ggplot(aes(x=TDS, y=SSBo))+ 
  geom_point(size=0.5) + 
  geom_linerange(aes(x = TDS, ymin = SSBo.lower, ymax = SSBo.upper),
                 colour = "black") +
  xlab("Total dissolved solids") + 
  ylab("SSBo (kg/ha)") + 
  ggsidekick::theme_sleek()+
  theme(
    axis.text.x = element_text(size = 8),
    axis.text.y = element_text(size = 8),
    axis.title = element_text(size = 10, colour = "grey30")
  )  
p2

p3 <- out %>%
  ggplot(aes(x=TS, y=SSBo))+ 
  geom_violin()+
  geom_point(size=0.5, position = position_jitter(w = 0.1, h = 0)) + 
  xlab("Trophic status") + 
  ylab("SSBo (kg/ha)") + 
  ggsidekick::theme_sleek()+
  theme(
    axis.text.x = element_text(size = 8),
    axis.text.y = element_text(size = 8),
    axis.title = element_text(size = 10, colour = "grey30")
  )  
p3

p4 <- out %>%
  ggplot(aes(x=DD5, y=SSBo))+ 
  geom_point(size=0.5) + 
  geom_linerange(aes(x = DD5, ymin = SSBo.lower, ymax = SSBo.upper),
                 colour = "black", size=0.5) +
  xlab("GDD > 5 (air temperature proxy)") + 
  ylab("SSBo (kg/ha)") + 
  ggsidekick::theme_sleek()+
  theme(
    axis.text.x = element_text(size = 8),
    axis.text.y = element_text(size = 8),
    axis.title = element_text(size = 10, colour = "grey30")
  )  
p4

p5 <- out %>%
  ggplot(aes(x=log(Area_Ha), y=SSBo))+ 
  geom_point(size=0.5) + 
  geom_linerange(aes(x = log(Area_Ha), ymin = SSBo.lower, ymax = SSBo.upper),
                 colour = "black", size=0.5) +
  xlab("ln( Lake area (ha) )") + 
  ylab("SSBo (kg/ha)") + 
  ggsidekick::theme_sleek()+
  theme(
    axis.text.x = element_text(size = 8),
    axis.text.y = element_text(size = 8),
    axis.title = element_text(size = 10, colour = "grey30")
  )  
p5

p6 <- out %>%
  ggplot(aes(x=TP, y=sbr0_kick))+ 
  geom_point(size=0.5) + 
  xlab("Total Phosphorous (mg/l)") + 
  ylab("sbro") + 
  ggsidekick::theme_sleek()+
  theme(
    axis.text.x = element_text(size = 8),
    axis.text.y = element_text(size = 8),
    axis.title = element_text(size = 10, colour = "grey30")
  )  

p6 

p7 <- out %>%
  ggplot(aes(x=Chloro, y=sbr0_kick))+ 
  geom_point(size=0.5) + 
  xlab("Chlorophyll a") + 
  ylab("sbro") + 
  ggsidekick::theme_sleek()+
  theme(
    axis.text.x = element_text(size = 8),
    axis.text.y = element_text(size = 8),
    axis.title = element_text(size = 10, colour = "grey30")
  )  

p7 

p8 <- out %>%
  ggplot(aes(x=TDS, y=sbr0_kick))+ 
  geom_point(size=0.5) + 
  xlab("Total dissolved solids") + 
  ylab("sbro") + 
  ggsidekick::theme_sleek()+
  theme(
    axis.text.x = element_text(size = 8),
    axis.text.y = element_text(size = 8),
    axis.title = element_text(size = 10, colour = "grey30")
  )  
p8

p9 <- out %>%
  ggplot(aes(x=TS, y=sbr0_kick))+ 
  geom_violin()+
  geom_point(size=0.5, position = position_jitter(w = 0.1, h = 0)) + 
  xlab("Trophic status") + 
  ylab("sbro") + 
  ggsidekick::theme_sleek()+
  theme(
    axis.text.x = element_text(size = 8),
    axis.text.y = element_text(size = 8),
    axis.title = element_text(size = 10, colour = "grey30")
  )  
p9

p10 <- out %>%
  ggplot(aes(x=DD5, y=sbr0_kick))+ 
  geom_point(size=0.5) + 
  xlab("GDD > 5 (air temperature proxy)") + 
  ylab("sbro") + 
  ggsidekick::theme_sleek()+
  theme(
    axis.text.x = element_text(size = 8),
    axis.text.y = element_text(size = 8),
    axis.title = element_text(size = 10, colour = "grey30")
  )  
p10

p11 <- out %>%
  ggplot(aes(x=log(Area_Ha), y=sbr0_kick))+ 
  geom_point(size=0.5) + 
  xlab("ln( Lake area (ha) )") + 
  ylab("sbro") + 
  ggsidekick::theme_sleek()+
  theme(
    axis.text.x = element_text(size = 8),
    axis.text.y = element_text(size = 8),
    axis.title = element_text(size = 10, colour = "grey30")
  )  
p11

p12 <- out %>%
  ggplot(aes(x=TP, y=ar))+ 
  geom_point(size=0.5) + 
  geom_linerange(aes(x = TP, ymin = ar.lower, ymax = ar.upper),
                 colour = "black") +
  xlab("Total Phosphorous (mg/l)") + 
  ylab(expression(ln~alpha)) + 
  ggsidekick::theme_sleek()+
  theme(
    axis.text.x = element_text(size = 8),
    axis.text.y = element_text(size = 8),
    axis.title = element_text(size = 10, colour = "grey30")
  )  

p12 

p13 <- out %>%
  ggplot(aes(x=Chloro, y=ar))+ 
  geom_point(size=0.5) + 
  geom_linerange(aes(x = Chloro, ymin = ar.lower, ymax = ar.upper),
                 colour = "black") +
  xlab("Chlorophyll a") + 
  ylab(expression(ln~alpha)) + 
  ggsidekick::theme_sleek()+
  theme(
    axis.text.x = element_text(size = 8),
    axis.text.y = element_text(size = 8),
    axis.title = element_text(size = 10, colour = "grey30")
  )  

p13 

p14 <- out %>%
  ggplot(aes(x=TDS, y=ar))+ 
  geom_point(size=0.5) + 
  geom_linerange(aes(x = TDS, ymin = ar.lower, ymax = ar.upper),
                 colour = "black") +
  xlab("Total dissolved solids") + 
  ylab(expression(ln~alpha)) + 
  ggsidekick::theme_sleek()+
  theme(
    axis.text.x = element_text(size = 8),
    axis.text.y = element_text(size = 8),
    axis.title = element_text(size = 10, colour = "grey30")
  )  
p14

p15 <- out %>%
  ggplot(aes(x=TS, y=ar))+ 
  geom_violin()+
  geom_point(size=0.5, position = position_jitter(w = 0.1, h = 0)) + 
  xlab("Trophic status") + 
  ylab(expression(ln~alpha)) + 
  ggsidekick::theme_sleek()+
  theme(
    axis.text.x = element_text(size = 8),
    axis.text.y = element_text(size = 8),
    axis.title = element_text(size = 10, colour = "grey30")
  )  
p15

p16 <- out %>%
  ggplot(aes(x=DD5, y=ar))+ 
  geom_point(size=0.5) + 
  geom_linerange(aes(x = DD5, ymin = ar.lower, ymax = ar.upper),
                 colour = "black", size=0.5) +
  xlab("GDD > 5 (air temperature proxy)") + 
  ylab(expression(ln~alpha)) + 
  ggsidekick::theme_sleek()+
  theme(
    axis.text.x = element_text(size = 8),
    axis.text.y = element_text(size = 8),
    axis.title = element_text(size = 10, colour = "grey30")
  )  
p16

p17 <- out %>%
  ggplot(aes(x=log(Area_Ha), y=ar))+ 
  geom_point(size=0.5) + 
  geom_linerange(aes(x = log(Area_Ha), ymin = ar.lower, ymax = ar.upper),
                 colour = "black", size=0.5) +
  xlab("ln( Lake area (ha) )") + 
  ylab(expression(ln~alpha)) + 
  ggsidekick::theme_sleek()+
  theme(
    axis.text.x = element_text(size = 8),
    axis.text.y = element_text(size = 8),
    axis.title = element_text(size = 10, colour = "grey30")
  )  
p17

left <- cowplot::plot_grid(p, p1, p2, p4, p5, p3,
                              nrow = 6
)

middle <-  cowplot::plot_grid(p6, p7, p8, p10, p11, p9,
                                 nrow = 6
)

right <-  cowplot::plot_grid(p12, p13, p14, p16, p17, p15,
                              nrow = 6
)

all <- cowplot::plot_grid(left, middle, right, ncol=3)

ggsave("plots/stuff_v_H20quality.pdf",
       width = 8,
       height = 11
)

ggsave("plots/stuff_v_H20quality.png",
       width = 8,
       height = 11,
       dpi = 2000
)


####Sbro_kick vs. all the h20 stuff

p <- out %>%
  ggplot(aes(x=TP, y=sbr0_kick))+ 
  geom_point(size=1.0) + 
  geom_linerange(aes(x = TP, ymin =sbr0_kick.lower, ymax = sbr0_kick.upper),
                 colour = "black") +
  xlab("Total Phosphorous (mg/l)") + 
  ylab("sbro") + 
  ggsidekick::theme_sleek()+
  theme(
    axis.text.x = element_text(size = 8),
    axis.text.y = element_text(size = 8),
    axis.title = element_text(size = 12, colour = "grey30")
  )  

p 

p1 <- out %>%
  ggplot(aes(x=Chloro, y=sbr0_kick))+ 
  geom_point(size=1.0) + 
  geom_linerange(aes(x = Chloro, ymin = sbr0_kick.lower, ymax = sbr0_kick.upper),
                 colour = "black") +
  xlab("chlorophyll-a") + 
  ylab("sbr0") + 
  ggsidekick::theme_sleek()+
  theme(
    axis.text.x = element_text(size = 8),
    axis.text.y = element_text(size = 8),
    axis.title = element_text(size = 12, colour = "grey30")
  )  

p1 

p2 <- out %>%
  ggplot(aes(x=TDS, y=sbr0_kick))+ 
  geom_point(size=1.0) + 
  geom_linerange(aes(x = TDS, ymin = sbr0_kick.lower, ymax = sbr0_kick.upper),
                 colour = "black") +
  xlab("Total dissolved solids") + 
  ylab("sbro") + 
  ggsidekick::theme_sleek()+
  theme(
    axis.text.x = element_text(size = 8),
    axis.text.y = element_text(size = 8),
    axis.title = element_text(size = 12, colour = "grey30")
  )  
p2

p3 <- out %>%
  ggplot(aes(x=TS, y=sbr0_kick))+ 
  geom_violin()+
  geom_point(position = position_jitter(w = 0.1, h = 0)) + 
  xlab("Trophic status") + 
  ylab("sbro") + 
  ggsidekick::theme_sleek()+
  theme(
    axis.text.x = element_text(size = 8),
    axis.text.y = element_text(size = 8),
    axis.title = element_text(size = 12, colour = "grey30")
  )  
p3

p4 <- out %>%
  ggplot(aes(x=DD5, y=sbr0_kick))+ 
  geom_point(position = position_jitter(w = 0.1, h = 0)) + 
  xlab("GDD > 5 (air temperature proxy)") + 
  ylab("sbro") + 
  ggsidekick::theme_sleek()+
  theme(
    axis.text.x = element_text(size = 8),
    axis.text.y = element_text(size = 8),
    axis.title = element_text(size = 12, colour = "grey30")
  )  
p4

my_plot <- cowplot::plot_grid(p, p1, p2, p3,  
                              nrow = 4
)

ggsave("plots/sbro_v_H20quality.pdf",
       width = 5,
       height = 8
)

ggsave("plots/sbro_v_H20quality.png",
       width = 5,
       height = 8,
       dpi = 2000
)








#------------------------------------
#----------------------
# Figure 4: Maps of R0, Flate, Fmsy, 
# This is some filthy trickery to manipulate massive stan fits
out <- hogzilla_list %>% map_dfr(function(hogzilla_list){
  hogzilla_list %>%
    spread_draws(R0[lake]) %>%
    median_qi() %>%
    mutate(
      value = R0
    )
})

out2 <- hogzilla_list %>% map_dfr(function(hogzilla_list){
  hogzilla_list %>%
    spread_draws(SPR[lake]) %>%
    median_qi() %>%
    mutate(
      value = SPR
    )
})

out3 <- hogzilla_list %>% map_dfr(function(hogzilla_list){
  hogzilla_list %>%
    spread_draws(sbr0_kick[lake]) %>%
    median_qi() %>%
    mutate(
      value = sbr0_kick
    )
})

out4 <- hogzilla_list %>% map_dfr(function(hogzilla_list){
  hogzilla_list %>%
    spread_draws(v[lake,period]) %>%
    median_qi() %>%
    mutate(
      value = v
    ) %>%
    filter(period==2)
})

out5 <- hogzilla_list %>% map_dfr(function(hogzilla_list){
  hogzilla_list %>%
    spread_draws(v[lake,period]) %>%
    median_qi() %>%
    mutate(
      value = v
    ) %>%
    filter(period==1)
})

out6 <- hogzilla_list %>% map_dfr(function(hogzilla_list){
  hogzilla_list %>%
    spread_draws(ar[lake]) %>%
    median_qi() %>%
    mutate(
      value = ar
    )
})

#bookkeeping on lake numbers and names
#Delete second to last lake, this was because ran lakes in 2's 
#(i.e., lake in rows 54/55 are the same)
out <- out[-55,]
out2 <- out2[-55,]
out3 <- out3[-55,]
out4 <- out4[-55,]
out5 <- out5[-55,]
out6 <- out6[-55,]

out$lake <- out2$lake <- out3$lake <-  
  out4$lake <- out5$lake <- out6$lake <- 1:55
out$name <- out2$name <- out3$name <- 
  out4$name <- out5$name <-
  out6$name <- unique(data$name)

data2 <- data %>% dplyr::select(name, X_long, Y_lat, Area_Ha, DD5, Mean_Depth, 
                                X_TTM_c, Y_TTM_c)
data2 <- unique(data2)

out <- left_join(out, data2, by="name")
out2 <- left_join(out2, data2, by="name")
out3 <- left_join(out3, data2, by="name")
out4 <- left_join(out4, data2, by="name")
out5 <- left_join(out5, data2, by="name")
out6 <- left_join(out6, data2, by="name")

#Let's get mappy
can1 <- raster::getData("GADM", country = "CAN", level = 1)
alta <- can1[can1$NAME_1 %in% "Alberta", ]

alta <- spTransform(
  alta,
  CRS("+proj=longlat +datum=WGS84")
)
alta.fort <- fortify(alta)

names(alta.fort)[1] <- "X_long"
names(alta.fort)[2] <- "Y_lat"

#----------------------------------
#R0 map
R0_map <- ggplot(NULL) +
  geom_polygon(colour = "black", fill = "white", 
               data = alta.fort, aes(x = X_long, y = Y_lat, group = id)) +
  geom_point(pch = 21, size = 1.5, data = out, aes(X_long, Y_lat, fill = R0)) +
  scale_x_continuous(breaks = c(-120, -115, -110)) +
  scale_y_continuous(breaks = c(49, 52, 56, 60)) +
  scale_fill_gradientn(
    colours = c("darkorange1", "white", "steelblue"),
    values = c(1, 0.5, 0.35, 0.1, 0),
    name = bquote(R0)
  )

R0_map <- R0_map + 
  ylab("Latitude") + xlab("Longitude")

R0_map <- R0_map + ggalt::coord_proj(
  paste0(CRS("+proj=tmerc +lat_0=0 +lon_0=-115 +k=0.9992 +x_0=500000 +y_0=0 +ellps=GRS80 +datum=NAD83 +units=m +no_defs"))
)

R0_map <- R0_map + ggsidekick::theme_sleek() +
  theme(
    legend.position="top",
    axis.line = element_line(colour = "grey30"),
    axis.title = element_text(size = 12, colour = "grey30"),
    strip.background = element_blank(),
    strip.text.x = element_blank(),
    panel.spacing.x = unit(1, "lines"),
    panel.spacing.y = unit(0.5, "lines"),
    panel.border = element_blank()
  )
R0_map

#----------------------------------
#SPR map

SPR_map <- ggplot(NULL) +
  geom_polygon(colour = "black", fill = "white", 
               data = alta.fort, aes(x = X_long, y = Y_lat, group = id)) +
  geom_point(pch = 21, size = 1.5, data = out2, aes(X_long, Y_lat, fill = SPR)) +
  scale_x_continuous(breaks = c(-120, -115, -110)) +
  scale_y_continuous(breaks = c(49, 52, 56, 60)) +
  scale_fill_gradientn(
    colours = c("darkorange1", "white", "steelblue"),
    values = c(1, 0.5, 0.35, 0.1, 0),
    name = bquote(SPR)
  )

SPR_map <- SPR_map + 
  ylab("Latitude") + xlab("Longitude")

SPR_map <- SPR_map + ggalt::coord_proj(
  paste0(CRS("+proj=tmerc +lat_0=0 +lon_0=-115 +k=0.9992 +x_0=500000 +y_0=0 +ellps=GRS80 +datum=NAD83 +units=m +no_defs"))
)

SPR_map <- SPR_map + ggsidekick::theme_sleek() +
  theme(
    legend.position="top",
    axis.line = element_line(colour = "grey30"),
    axis.title = element_text(size = 12, colour = "grey30"),
    strip.background = element_blank(),
    strip.text.x = element_blank(),
    panel.spacing.x = unit(1, "lines"),
    panel.spacing.y = unit(0.5, "lines"),
    panel.border = element_blank()
  )
SPR_map

#----------------------------------
#sbro map

sbro_map <- ggplot(NULL) +
  geom_polygon(colour = "black", fill = "white", 
               data = alta.fort, aes(x = X_long, y = Y_lat, group = id)) +
  geom_point(pch = 21, size = 1.5, data = out3, aes(X_long, Y_lat, fill = sbr0_kick)) +
  scale_x_continuous(breaks = c(-120, -115, -110)) +
  scale_y_continuous(breaks = c(49, 52, 56, 60)) +
  scale_fill_gradientn(
    colours = c("darkorange1", "white", "steelblue"),
    values = c(1, 0.5, 0.35, 0.1, 0),
    name = bquote(sbro)
  )

sbro_map <- sbro_map + 
  ylab("Latitude") + xlab("Longitude")

sbro_map <- sbro_map + ggalt::coord_proj(
  paste0(CRS("+proj=tmerc +lat_0=0 +lon_0=-115 +k=0.9992 +x_0=500000 +y_0=0 +ellps=GRS80 +datum=NAD83 +units=m +no_defs"))
)

sbro_map <- sbro_map + ggsidekick::theme_sleek() +
  theme(
    legend.position="top",
    axis.line = element_line(colour = "grey30"),
    axis.title = element_text(size = 12, colour = "grey30"),
    strip.background = element_blank(),
    strip.text.x = element_blank(),
    panel.spacing.x = unit(1, "lines"),
    panel.spacing.y = unit(0.5, "lines"),
    panel.border = element_blank()
  )
sbro_map

#----------------------------------
#Flate map

Flate_map <- ggplot(NULL) +
  geom_polygon(colour = "black", fill = "white", 
               data = alta.fort, aes(x = X_long, y = Y_lat, group = id)) +
  geom_point(pch = 21, size = 1.5, data = out4, aes(X_long, Y_lat, fill = v)) +
  scale_x_continuous(breaks = c(-120, -115, -110)) +
  scale_y_continuous(breaks = c(49, 52, 56, 60)) +
  scale_fill_gradientn(
    colours = c("darkorange1", "white", "steelblue"),
    values = c(1, 0.75, 0.65, 0.25, 0),
    name = bquote(Flate)
  )

Flate_map <- Flate_map + 
  ylab("Latitude") + xlab("Longitude")

Flate_map <- Flate_map + ggalt::coord_proj(
  paste0(CRS("+proj=tmerc +lat_0=0 +lon_0=-115 +k=0.9992 +x_0=500000 +y_0=0 +ellps=GRS80 +datum=NAD83 +units=m +no_defs"))
)

Flate_map <- Flate_map + ggsidekick::theme_sleek() +
  theme(
    legend.position="top", 
    axis.line = element_line(colour = "grey30"),
    axis.title = element_text(size = 12, colour = "grey30"),
    strip.background = element_blank(),
    strip.text.x = element_blank(),
    panel.spacing.x = unit(1, "lines"),
    panel.spacing.y = unit(0.5, "lines"),
    panel.border = element_blank()
  )
Flate_map

#----------------------------------
#Fearly map

Fearly_map <- ggplot(NULL) +
  geom_polygon(colour = "black", fill = "white", 
               data = alta.fort, aes(x = X_long, y = Y_lat, group = id)) +
  geom_point(pch = 21, size = 1.5, data = out5, aes(X_long, Y_lat, fill = v)) +
  scale_x_continuous(breaks = c(-120, -115, -110)) +
  scale_y_continuous(breaks = c(49, 52, 56, 60)) +
  scale_fill_gradientn(
    colours = c("darkorange1", "white", "steelblue"),
    values = c(1, 0.75, 0.65, 0.25, 0),
    name = bquote(Fearly)
  )

Fearly_map <- Fearly_map + 
  ylab("Latitude") + xlab("Longitude")

Fearly_map <- Fearly_map + ggalt::coord_proj(
  paste0(CRS("+proj=tmerc +lat_0=0 +lon_0=-115 +k=0.9992 +x_0=500000 +y_0=0 +ellps=GRS80 +datum=NAD83 +units=m +no_defs"))
)

Fearly_map <- Fearly_map + ggsidekick::theme_sleek() +
  theme(
    legend.position="top", 
    axis.line = element_line(colour = "grey30"),
    axis.title = element_text(size = 12, colour = "grey30"),
    strip.background = element_blank(),
    strip.text.x = element_blank(),
    panel.spacing.x = unit(1, "lines"),
    panel.spacing.y = unit(0.5, "lines"),
    panel.border = element_blank()
  )
Fearly_map

#----------------------------------
#ar map

ar_map <- ggplot(NULL) +
  geom_polygon(colour = "black", fill = "white", 
               data = alta.fort, aes(x = X_long, y = Y_lat, group = id)) +
  geom_point(pch = 21, size = 1.5, data = out6, aes(X_long, Y_lat, fill = ar)) +
  scale_x_continuous(breaks = c(-120, -115, -110)) +
  scale_y_continuous(breaks = c(49, 52, 56, 60)) +
  scale_fill_gradientn(
    colours = c("darkorange1", "white", "steelblue"),
    values = c(1, 0.75, 0.5, 0.25, 0),
    name = bquote(ar)
  )

ar_map <- ar_map + 
  ylab("Latitude") + xlab("Longitude")

ar_map <- ar_map + ggalt::coord_proj(
  paste0(CRS("+proj=tmerc +lat_0=0 +lon_0=-115 +k=0.9992 +x_0=500000 +y_0=0 +ellps=GRS80 +datum=NAD83 +units=m +no_defs"))
)

ar_map <- ar_map + ggsidekick::theme_sleek() +
  theme(
    legend.position="top", 
    axis.line = element_line(colour = "grey30"),
    axis.title = element_text(size = 12, colour = "grey30"),
    strip.background = element_blank(),
    strip.text.x = element_blank(),
    panel.spacing.x = unit(1, "lines"),
    panel.spacing.y = unit(0.5, "lines"),
    panel.border = element_blank()
  )
ar_map

#-----------------
#carrying capacity map
out$ssbo <- out$R0*out3$sbr0_kick

K_map <- ggplot(NULL) +
  geom_polygon(colour = "black", fill = "white", 
               data = alta.fort, aes(x = X_long, y = Y_lat, group = id)) +
  geom_point(pch = 21, size = 1.5, data = out, aes(X_long, Y_lat, fill = ssbo)) +
  scale_x_continuous(breaks = c(-120, -115, -110)) +
  scale_y_continuous(breaks = c(49, 52, 56, 60)) +
  scale_fill_gradientn(
    colours = c("darkorange1", "white", "steelblue"),
    values = c(1, 0.5, 0.15, 0.1, 0),
    name = bquote(SSBo)
  )

K_map <- K_map + 
  ylab("Latitude") + xlab("Longitude")

K_map <- K_map + ggalt::coord_proj(
  paste0(CRS("+proj=tmerc +lat_0=0 +lon_0=-115 +k=0.9992 +x_0=500000 +y_0=0 +ellps=GRS80 +datum=NAD83 +units=m +no_defs"))
)

K_map <- K_map + ggsidekick::theme_sleek() +
  theme(
    legend.position="top", 
    axis.line = element_line(colour = "grey30"),
    axis.title = element_text(size = 12, colour = "grey30"),
    strip.background = element_blank(),
    strip.text.x = element_blank(),
    panel.spacing.x = unit(1, "lines"),
    panel.spacing.y = unit(0.5, "lines"),
    panel.border = element_blank()
  )
K_map

#-----------------
#Make a panel of maps:

my_map <- cowplot::plot_grid(R0_map, sbro_map, K_map,   
                             ar_map, Fearly_map, Flate_map, 
                             SPR_map, 
                             nrow = 3
)
ggsave("plots/S4.pdf",
       width = 6.75,
       height = 9
)

ggsave("plots/S4.png",
       width = 6.75,
       height = 9,
       dpi = 2000
)


#-----------------
#Look for spatial correlation trickery
#put distance in km
out2[,c("Y_TTM_c","X_TTM_c")] <- out2[,c("Y_TTM_c","X_TTM_c")] / 1000 

Loc <- unique(out2[,c("Y_TTM_c","X_TTM_c")])
my_data <- data.frame(Fmsy = out2$Fmsy, Loc)
coordinates(my_data)= ~X_TTM_c + Y_TTM_c

my_variogram <- variogram(Fmsy~1, data=my_data, alpha=c(0,45,90,135))
plot(my_variogram)
my_variogram #np=#points, dist=avg distance for that lag, gamma=mean for lag

my_vario_model <- vgm(model="Gau", psill=0.0015, range=30)

plot(my_variogram, model=my_vario_model)

my_vario_fit <- fit.variogram(my_variogram, model=my_vario_model)
plot(my_variogram, model=my_vario_fit)
my_vario_fit


summary(my_vario_fit)


?cor
cov(my_data$Fmsy, my_data$Fmsy)

D <- as.matrix(dist(Loc))
dis.vec <- seq(0, max(D), length = 1000)

vario.a <- variog(data = as.vector(Fmsy), coords = as.matrix(Loc), max.dist=100, 
                  estimator.type = "modulus")
plot(vario.a) 


vario.b <- variog(data = as.vector(Fmsy), coords = as.matrix(Loc), max.dist=100, bin.cloud=TRUE)
plot(vario.b) 


library(gstat)
v = variogram(log(zinc)~1, meuse)
v.fit = fit.variogram(v, vgm(1, "Sph", 900, 1))
v.fit
out2 %>%
ggplot(aes(x=X_TTM_c, y=Fmsy)) + geom_point()

out2 %>%
ggplot(aes(x=Y_TTM_c, y=Fmsy)) + geom_point()

#
#----------------------------------
#Time series clustering analysis
out <- out %>% filter(year <= 2018) %>%
  filter(name != "lac bellevue") %>%
  filter(name != "sturgeon lake") %>%
  select(year, R2, name)

my_data <- out %>% spread(name, R2)

my_data <- as.matrix(my_data)
rownames(my_data) <- my_data[,"year"]
my_data <- my_data[,-1]

df2 <- data.frame(t(my_data))

tree <- hclust(dist(df2))

out_wide <- out %>% pivot_wider(names_from=year, values_from=R2) %>%
  mutate_at(vars(-name), as.numeric) %>% ungroup()

wss <- map_dbl(1:5, ~{kmeans(select(out_wide, -name), ., nstart=50,iter.max = 15 )$tot.withinss})
n_clust <- 1:5
elbow_df <- as.data.frame(cbind("n_clust" = n_clust, "wss" = wss))
ggplot(elbow_df) +
  geom_line(aes(y = wss, x = n_clust), colour = "#82518c") +
  ggsidekick::theme_sleek()

clusters <- kmeans(select(out_wide, -name), centers=2)

centers <- rownames_to_column(as.data.frame(clusters$centers), "cluster")

out_wide <- out_wide %>%
  mutate(cluster=clusters$cluster)

out_long <- out_wide %>%
  pivot_longer(cols=c(-name, -cluster), names_to = "year", values_to="R2")

centers_long <- centers %>%
  pivot_longer(cols = -cluster, names_to = "year", values_to = "R2") 

p <- ggplot() +
  geom_line(data = out_long, aes(y = R2, x = as.numeric(year), group = name), colour = "steelblue") +
  facet_wrap(~cluster, ncol = 1) + 
  geom_line(data = centers_long, aes(y = R2, x = as.numeric(year), group = cluster), col = "darkorange2", size = 2) +
  ggsidekick::theme_sleek() +
  scale_x_continuous(breaks = c(1980, 1990, 2000, 2010, 2018), 
                     limits=c(1980,2018)) +
  xlab("Year") + ylab("Age 2 Walleye") + 
  theme(
    legend.position="none", 
    axis.line = element_line(colour = "grey30"),
    axis.title = element_text(size = 12, colour = "grey30"),
    strip.text.x = element_text(size=8, colour = "grey30"),
    panel.spacing.x = unit(1, "lines"),
    panel.spacing.y = unit(0.5, "lines"),
    panel.border = element_blank(),
    axis.text.x = element_text(angle = 90, size=10),
    axis.text.y = element_text(size=10)
  ) +
  geom_vline(xintercept=2000)

p <- p + geom_vline(xintercept = 1993) +
  geom_vline(xintercept = 2008)+
  geom_vline(xintercept = 2014)

ggsave("plots/clusters_noweirdlakesv3.pdf",
       width = 4,
       height = 7
)

unique(out_long[which(out_long$cluster==2),"name"])

df2$gp <- cutree(tree, k=25)


install.packages("rsoi")
library(rsoi)

soi <- download_soi()
soi

soi <- soi %>% filter(Year >= 1980) %>%
  filter(Year <=2018) 

soi <- soi %>%
  ggplot(aes(x=Date, y=SOI_3MON_AVG)) + 
  geom_point() + 
  geom_line() +
  geom_vline(xintercept=as.numeric(as.Date("1998-01-01")), linetype=4) + 
  geom_vline(xintercept=as.numeric(as.Date("1998-12-01")), linetype=4) + 
  geom_vline(xintercept=as.numeric(as.Date("1991-01-01")), linetype=4) +
  geom_vline(xintercept=as.numeric(as.Date("1991-12-01")), linetype=4) +
  geom_vline(xintercept=as.numeric(as.Date("2006-01-01")), linetype=4) +
  geom_vline(xintercept=as.numeric(as.Date("2006-12-01")), linetype=4) +
  geom_vline(xintercept=as.numeric(as.Date("2012-01-01")), linetype=4) +
  geom_vline(xintercept=as.numeric(as.Date("2012-12-01")), linetype=4)

enso <- download_enso(climate_idx = 'npgo')
enso <- enso %>% filter(Year >= 1980) %>%
  filter(Year <=2018) 

enso <- enso %>%
  ggplot(aes(x=Date, y=NPGO)) + 
  geom_point() + 
  geom_line() +
  geom_vline(xintercept=as.numeric(as.Date("1998-01-01")), linetype=4) + 
  geom_vline(xintercept=as.numeric(as.Date("1998-12-01")), linetype=4) + 
  geom_vline(xintercept=as.numeric(as.Date("1991-01-01")), linetype=4) +
  geom_vline(xintercept=as.numeric(as.Date("1991-12-01")), linetype=4) +
  geom_vline(xintercept=as.numeric(as.Date("2006-01-01")), linetype=4) +
  geom_vline(xintercept=as.numeric(as.Date("2006-12-01")), linetype=4) +
  geom_vline(xintercept=as.numeric(as.Date("2012-01-01")), linetype=4) +
  geom_vline(xintercept=as.numeric(as.Date("2012-12-01")), linetype=4)

oni <- download_enso(climate_idx = 'oni')
oni <- oni %>% filter(Year >= 1980) %>%
  filter(Year <=2018) 

oni <- oni %>%
  ggplot(aes(x=Date, y=ONI)) + 
  geom_point() + 
  geom_line() +
  geom_vline(xintercept=as.numeric(as.Date("1998-01-01")), linetype=4) + 
  geom_vline(xintercept=as.numeric(as.Date("1998-12-01")), linetype=4) + 
  geom_vline(xintercept=as.numeric(as.Date("1991-01-01")), linetype=4) +
  geom_vline(xintercept=as.numeric(as.Date("1991-12-01")), linetype=4) +
  geom_vline(xintercept=as.numeric(as.Date("2006-01-01")), linetype=4) +
  geom_vline(xintercept=as.numeric(as.Date("2006-12-01")), linetype=4) +
  geom_vline(xintercept=as.numeric(as.Date("2012-01-01")), linetype=4) +
  geom_vline(xintercept=as.numeric(as.Date("2012-12-01")), linetype=4)

npgo <- download_enso(climate_idx = 'npgo')
npgo <- npgo %>% filter(Year >= 1980) %>%
  filter(Year <=2018) 

npgo <- npgo %>%
  ggplot(aes(x=Date, y=NPGO)) + 
  geom_point() + 
  geom_line() +
  geom_vline(xintercept=as.numeric(as.Date("1998-01-01")), linetype=4) + 
  geom_vline(xintercept=as.numeric(as.Date("1998-12-01")), linetype=4) + 
  geom_vline(xintercept=as.numeric(as.Date("1991-01-01")), linetype=4) +
  geom_vline(xintercept=as.numeric(as.Date("1991-12-01")), linetype=4) +
  geom_vline(xintercept=as.numeric(as.Date("2006-01-01")), linetype=4) +
  geom_vline(xintercept=as.numeric(as.Date("2006-12-01")), linetype=4) +
  geom_vline(xintercept=as.numeric(as.Date("2012-01-01")), linetype=4) +
  geom_vline(xintercept=as.numeric(as.Date("2012-12-01")), linetype=4)

pdo <- download_pdo()
pdo <- pdo %>% filter(Year >= 1980) %>%
  filter(Year <=2018) 

pdo <- pdo %>%
  ggplot(aes(x=Date, y=PDO)) + 
  geom_point() + 
  geom_line() +
  geom_vline(xintercept=as.numeric(as.Date("1998-01-01")), linetype=4) + 
  geom_vline(xintercept=as.numeric(as.Date("1998-12-01")), linetype=4) + 
  geom_vline(xintercept=as.numeric(as.Date("1991-01-01")), linetype=4) +
  geom_vline(xintercept=as.numeric(as.Date("1991-12-01")), linetype=4) +
  geom_vline(xintercept=as.numeric(as.Date("2006-01-01")), linetype=4) +
  geom_vline(xintercept=as.numeric(as.Date("2006-12-01")), linetype=4) +
  geom_vline(xintercept=as.numeric(as.Date("2012-01-01")), linetype=4) +
  geom_vline(xintercept=as.numeric(as.Date("2012-12-01")), linetype=4)
  
climate <- cowplot::plot_grid(soi, oni, enso, npgo, pdo,
                             ncol = 1
)

climate

ggsave("plots/climate.pdf",
       width = 8,
       height = 11
)



  select(Month="Jan")


summary(tree)
plot(tree)

df2 <- df2 %>%
  mutate(name=rownames(.)) %>%
  select(name, gp)

df3 <- left_join(out, df2)

ggplot(df3, aes(as.numeric(year), R2, colour=name)) + 
  geom_line() + 
  facet_wrap(~gp) + 
  guides(colour=FALSE) 
  




df <- t(my_data)

tree <- hclust(dist(df))
plot(tree)
my_data$gp <- cutree(tree,k=3)


##################################################

bumpers <- c("buck lake", "calling lake", 
             "christina lake", "ioesegun lake", 
             "pigeon lake", "seibert lake", 
             "smoke lake", "spencer lake", 
             "sylvan lake")

out %>% filter(name %in% bumpers)

out$which <- ifelse(out$name %in% bumpers, "bump", "ugly")

out %>% 
  ggplot(aes(x=which, y=Area_Ha)) + 
  geom_boxplot()

out %>% 
  ggplot(aes(x=which, y=Mean_Depth)) + 
  geom_boxplot()

new_dat <- read.csv("C:/Users/Chris_Cahill/Desktop/pigeon_water.csv")

new_dat$Date <- as.Date(new_dat$Date)

p <- new_dat %>%
  ggplot(aes(x=Date, y=RelativeElevation )) + 
  geom_point() + 
  geom_line() +
  ylim(849, 851) + 
  geom_vline(xintercept=as.numeric(as.Date("1998-01-01")), linetype=4) + 
  geom_vline(xintercept=as.numeric(as.Date("1998-12-01")), linetype=4) + 
  #geom_vline(xintercept=as.numeric(as.Date("1991-01-01")), linetype=4) +
  #geom_vline(xintercept=as.numeric(as.Date("1991-12-01")), linetype=4) +
  geom_vline(xintercept=as.numeric(as.Date("2006-01-01")), linetype=4) +
  geom_vline(xintercept=as.numeric(as.Date("2006-12-01")), linetype=4) +
  geom_vline(xintercept=as.numeric(as.Date("2012-01-01")), linetype=4) +
  geom_vline(xintercept=as.numeric(as.Date("2012-12-01")), linetype=4)
p
