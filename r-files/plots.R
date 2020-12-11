library(tidyverse)
library(tidybayes)
library(rgdal)
#-------------------------
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

#----------------------
# Figure 4: Maps of R0, Flate, Fmsy, 
# This is some filthy trickery to manipulate 2.3 GB of stan fits
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
    spread_draws(Fmsy[lake]) %>%
    median_qi() %>%
    mutate(
      value = Fmsy
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

#Delete second to last lake, this was because ran lakes in 2's 
#(i.e., lake in rows 54/55 are the same)
out <- out[-55,]
out2 <- out2[-55,]
out3 <- out3[-55,]
out4 <- out4[-55,]

#bookkeeping on lake numbers and names
out$lake <- out2$lake <- out3$lake <-  out4$lake <- 1:55
out$name <- out2$name <- out3$name <- out4$name <- unique(data$name)

data2 <- data %>% dplyr::select(name, X_long, Y_lat)
data2 <- unique(data2)

out <- left_join(out, data2, by="name")
out2 <- left_join(out2, data2, by="name")
out3 <- left_join(out3, data2, by="name")
out4 <- left_join(out4, data2, by="name")

#Make some maps
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
#Fmsy map

Fmsy_map <- ggplot(NULL) +
  geom_polygon(colour = "black", fill = "white", 
               data = alta.fort, aes(x = X_long, y = Y_lat, group = id)) +
  geom_point(pch = 21, size = 1.5, data = out2, aes(X_long, Y_lat, fill = Fmsy)) +
  scale_x_continuous(breaks = c(-120, -115, -110)) +
  scale_y_continuous(breaks = c(49, 52, 56, 60)) +
  scale_fill_gradientn(
    colours = c("darkorange1", "white", "steelblue"),
    values = c(1, 0.5, 0.35, 0.1, 0),
    name = bquote(Fmsy)
  )

Fmsy_map <- Fmsy_map + 
  ylab("Latitude") + xlab("Longitude")

Fmsy_map <- Fmsy_map + ggalt::coord_proj(
  paste0(CRS("+proj=tmerc +lat_0=0 +lon_0=-115 +k=0.9992 +x_0=500000 +y_0=0 +ellps=GRS80 +datum=NAD83 +units=m +no_defs"))
)

Fmsy_map <- Fmsy_map + ggsidekick::theme_sleek() +
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
Fmsy_map

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
    values = c(1, 0.5, 0.35, 0.1, 0),
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

#-----------------
#Make a panel of maps:

my_map <- cowplot::plot_grid(R0_map, sbro_map, Fmsy_map, Flate_map,
                             nrow = 1
)
# ggsave("plots/Fig_4.pdf",
#        width = 7.25,
#        height = 3
# )
# 
# ggsave("plots/Fig_4.png",
#        width = 7.25,
#        height = 3,
#        dpi = 2000
# )

#----------------------
# Province-wide Kobe plot with Flate
# Figure 5

out <- hogzilla_list %>% map_dfr(function(hogzilla_list){
  hogzilla_list %>%
    spread_draws(b_ratio[lake], F_ratio[lake]) %>%
    median_qi() %>%
    mutate(
      value = b_ratio,
      value = F_ratio
    )
})

out <- out[-55,]
#bookkeeping on lake numbers and names
out$lake <- 1:55
out$name <- unique(data$name)

data2 <- data %>% dplyr::select(name, X_long, Y_lat)
data2 <- unique(data2)

out <- left_join(out, data2, by="name")

p <- out %>%
ggplot(aes(x = b_ratio, y = F_ratio)) +
  geom_point(pch = 21, fill="black", size = 1.0) +
  #geom_text(aes(label=name),hjust=0, vjust=0) + 
  ggsidekick::theme_sleek() +
  ylab("Flate / Fmsy") +
  xlab("Mean survey SSB / SSBo") +
  scale_x_continuous(breaks = seq(0, 25, 5)) +
  geom_hline(yintercept = 1, lty = 1, size = 0.75, alpha = 0.25) +
  geom_vline(xintercept = 0.4, lty = 1, size = 0.75, alpha = 0.25) +
  theme(
    axis.text.x = element_text(size = 10),
    axis.text.y = element_text(size = 10),
    axis.title = element_text(size = 12, colour = "grey30"),
  )

p <- p + ggrepel::geom_label_repel(aes(label = name),
                 segment.color = "grey50", 
                 label.size=NA)
p

ggsave("plots/alta_Kobe.png",
       width = 6,
       height = 5,
       dpi = 2000
)

ggsave("plots/alta_Kobe.pdf",
       width = 6,
       height = 5
)

#----------------------------------
#Flate map

bratio_map <- ggplot(NULL) +
  geom_polygon(colour = "black", fill = "white", 
               data = alta.fort, aes(x = X_long, y = Y_lat, group = id)) +
  geom_point(pch = 21, size = 1.5, data = out, aes(X_long, Y_lat, fill = b_ratio)) +
  scale_x_continuous(breaks = c(-120, -115, -110)) +
  scale_y_continuous(breaks = c(49, 52, 56, 60)) +
  scale_fill_gradientn(
    colours = c("darkorange1", "white", "steelblue"),
    values = c(1, 0.5, 0.35, 0.1, 0),
    name = bquote(Flate)
  )

bratio_map <- bratio_map + 
  ylab("Latitude") + xlab("Longitude")

bratio_map <- bratio_map + ggalt::coord_proj(
  paste0(CRS("+proj=tmerc +lat_0=0 +lon_0=-115 +k=0.9992 +x_0=500000 +y_0=0 +ellps=GRS80 +datum=NAD83 +units=m +no_defs"))
)

bratio_map <- bratio_map + ggsidekick::theme_sleek() +
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
bratio_map

#------------------------------------
out <- hogzilla_list %>% map_dfr(function(hogzilla_list){
  hogzilla_list %>%
    spread_draws(R0[lake], sbr0_kick[lake]) %>%
    median_qi() %>%
    mutate(
      value = R0,
      value = sbr0_kick
    )
})

out <- out[-55,]
#bookkeeping on lake numbers and names
out$lake <- 1:55
out$name <- unique(data$name)

data2 <- data %>% dplyr::select(name, X_long, Y_lat)
data2 <- unique(data2)

out <- left_join(out, data2, by="name")

out <- out %>% mutate(SSBo = R0*sbr0_kick)

map <- ggplot(NULL) +
  geom_polygon(colour = "black", fill = "white", 
               data = alta.fort, aes(x = X_long, y = Y_lat, group = id)) +
  geom_point(pch = 21, size = 1.5, data = out, aes(X_long, Y_lat, fill = SSBo)) +
  scale_x_continuous(breaks = c(-120, -115, -110)) +
  scale_y_continuous(breaks = c(49, 52, 56, 60)) +
  scale_fill_gradientn(
    colours = c("darkorange1", "white", "steelblue"),
    values = c(1, 0.5, 0.35, 0.1, 0),
    name = bquote(SSBo)
  )

map <- map + 
  ylab("Latitude") + xlab("Longitude")

map <- map + ggalt::coord_proj(
  paste0(CRS("+proj=tmerc +lat_0=0 +lon_0=-115 +k=0.9992 +x_0=500000 +y_0=0 +ellps=GRS80 +datum=NAD83 +units=m +no_defs"))
)

map <- map + ggsidekick::theme_sleek() +
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
map


#------------------------------------
out <- hogzilla_list %>% map_dfr(function(hogzilla_list){
  hogzilla_list %>%
    spread_draws(SSB_obs[Nobs]) %>%
    median_qi() %>%
    mutate(
      SSB_obs = SSB_obs,
      SSB_lower = SSB_obs - 0.20 * SSB_obs,
      SSB_upper = SSB_obs + 0.20 * SSB_obs
    )
})

#Get rid of the repeat years
out <- out[-(234:236),]
out$name <- data$name
out$year <- data$year

out <- out %>% group_by(name) %>%
  mutate(mean_ssb_c = mean(SSB_obs))

out2 <- hogzilla_list %>% map_dfr(function(hogzilla_list){
  hogzilla_list %>%
    spread_draws(R0[lake], sbr0_kick[lake]) %>%
    median_qi() %>%
    mutate(
      value = R0,
      value = sbr0_kick
    )
})

out2 <- out2[-55,]
#bookkeeping on lake numbers and names
out2$lake <- 1:55
out2$name <- unique(data$name)

data2 <- data %>% dplyr::select(name, X_long, Y_lat)
data2 <- unique(data2)

out2 <- left_join(out2, data2, by="name")

out2 <- out2 %>% mutate(SSBo = R0*sbr0_kick)

out <- left_join(out, out2, by="name")

out <- out %>% mutate(b_ratio = mean_ssb_c / SSBo)

p <- out %>%
  ggplot(aes(x = b_ratio, y = F_ratio)) +
  geom_point(pch = 21, fill="black", size = 1.0) +
  #geom_text(aes(label=name),hjust=0, vjust=0) + 
  ggsidekick::theme_sleek() +
  ylab("Flate / Fmsy") +
  xlab("Mean survey SSB / SSBo") +
  scale_x_continuous(breaks = seq(0, 25, 5)) +
  geom_hline(yintercept = 1, lty = 1, size = 0.75, alpha = 0.25) +
  geom_vline(xintercept = 0.4, lty = 1, size = 0.75, alpha = 0.25) +
  theme(
    axis.text.x = element_text(size = 10),
    axis.text.y = element_text(size = 10),
    axis.title = element_text(size = 12, colour = "grey30"),
  )

p <- p + ggrepel::geom_label_repel(aes(label = name),
                                   segment.color = "grey50", 
                                   label.size=NA)
p





SSB_C_dat$year <- stan_data$year + add_year
SSB_C_dat$name <- tolower(data$name)

trajectory_dat <- left_join(trajectory_dat, SSB_C_dat, 
                            by = c("name", "year"))

