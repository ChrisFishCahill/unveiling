#hierarchical clustering analysis on the recruitment posterior medians 
out <- hogzilla_list %>% map_dfr(function(hogzilla_list) {
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

# get rid of second to last lake which was fit twice:
out <- out[-c(2647:2695), ]

my_lakes <- rep(1:55, each = length(1980:2028))
my_names <- rep(unique(data$name), each = length(1980:2028))

out$lake <- my_lakes
out$name <- my_names

out <- out %>% filter(year <= 2018)

out_wide <- out %>%
  ungroup() %>%
  mutate(med = arm::rescale(med)) %>%
  select(name, med, year) %>%
  pivot_wider(names_from = year, values_from = med) %>%
  mutate_at(vars(-name), as.numeric) 

out_long <- out_wide %>%
  pivot_longer(cols=c(-name), names_to="year", 
               names_transform = list(year = as.numeric),  
               values_to="R2")
out_long %>% glimpse()

out_wide2 <- as.matrix(out_wide)

spread_out <- out_long %>%
  spread(name, R2)  %>%
  glimpse()

R2 <- t(spread_out[-1])


walleye_dist <- dist(R2, method="euclidean")

library(ggdendro)
library(dendextend)
library(RColorBrewer)
# 
# png("plots/cluster.png",
#     width = 11, height = 8, units = "in", res = 2000
# )
# 
# pdf("plots/cluster_centroid.pdf",
#     width = 8, height = 9
# )
# 
# par(mar=c(1,1,1,10))
# 
# walleye_dist %>% hclust(method="ward.D") %>%
#   as.dendrogram() %>%
#   set("labels_col", value = brewer.pal(8, name="Dark2"), k=8) %>%
#   #set("branches_k_color", brewer.pal(8, name="Dark2"), k = 8) %>%
#   plot(horiz=TRUE, axes=FALSE)
#   #rect.dendrogram(dend, k=3, lty = 5, lwd = 0, x=1, 
#    #                col=rgb(0.1, 0.2, 0.4, 0.1) ) 
# 
# dev.off()
# dev.off()

fit <- walleye_dist %>% hclust(method="ward.D")

clustered_data <- cutree(fit, k=8)
clustered_data_tidy <- as.data.frame(as.table(clustered_data)) %>% glimpse()
colnames(clustered_data_tidy) <- c("name","cluster")
clustered_data_tidy$name <- as.character(clustered_data_tidy$name)

joined_clusters <- out_long %>%
  inner_join(clustered_data_tidy, by = "name") %>%
  glimpse()

table(clustered_data_tidy$cluster)

cluster3 <- joined_clusters %>% filter(cluster == "3") 

joined_clusters <- joined_clusters %>%
  group_by(cluster, year) %>%
  mutate(med = median(R2)) %>%
  ungroup()

out$cluster <- joined_clusters$cluster
out <- out %>%
  group_by(cluster, year) %>%
  mutate(med_avg = median(med)) %>%
  ungroup()


out %>%
ggplot(aes(year, med, group=name, color=name)) +
  geom_line(color="steelblue", alpha=0.75) +
  geom_line(aes(y = med_avg, x = year, group = cluster),
            col = "darkorange2", size = 1.25)  + 
  theme_minimal() +
  ylab("R2") + xlab("Year") + 
  facet_wrap(~cluster, nrow = 4, ncol=2, scales="free_y") + 
  ggsidekick::theme_sleek() +
  scale_x_continuous(
    breaks = c(1980, 1990, 2000, 2010, 2018),
    limits = c(1980, 2018)
  ) +
  xlab("Year") +
  ylab("Age 2 Walleye") 

ggsave("plots/cluster_trends.pdf",
       width = 8,
       height = 11
)

########################################################
#Ridge plot by cluster?
# data <- readRDS("data/BERTA-wide-0-25.rds")
# 
# data$year <- data$year + 1999

join_data <- data %>% group_by(name) %>%
  mutate(max_year = max(year), 
         min_year = min(year)) %>%
  select(c("name", "max_year", "min_year"))

join_data <- unique(join_data)

out <- left_join(out, join_data, by="name")

out

my_early_mean <- out %>%
  group_by(lake, med) %>%
  filter(year < 1982) %>%
  mutate(my_early_mean = mean(med)) %>%
  ungroup() %>%
  select(c("name", "my_early_mean"))
my_early_mean <- unique(my_early_mean)


out <- left_join(out, my_early_mean)

out <- out %>% group_by(name) %>%
  mutate(med = ifelse(year <= max_year, med, NA)) 
out <- out %>% group_by(name) %>%
  mutate(med = ifelse(year <= min_year-18, NA, med))

find_min <- data %>%
  select(name, year, "2":"20") %>%
  pivot_longer(
    cols = c("2":"20"), names_to = "age",
    values_to="count"
  )

find_min$age = as.numeric(find_min$age)
find_min$earliest_yr <- NA

for(i in unique(find_min$name)){
  sub.dat <- find_min[which(find_min$name==i),]
  first_yr <- unique(sub.dat$year)[1]
  second_yr <- unique(sub.dat$year)[2]
  third_yr <- unique(sub.dat$year)[3]
  
  sub.sub.dat <- sub.dat[which(sub.dat$year==first_yr),]
  max_age <- max(sub.sub.dat$age[which(sub.sub.dat$count !=0)])
  
  sub.sub.dat2 <- sub.dat[which(sub.dat$year==second_yr),]
  max_age2 <- max(sub.sub.dat2$age[which(sub.sub.dat2$count !=0)])
  
  sub.sub.dat3 <- sub.dat[which(sub.dat$year==third_yr),]
  max_age3 <- max(sub.sub.dat3$age[which(sub.sub.dat3$count !=0)])
  
  hatch_yr1 <- first_yr - (max_age-2) #2 for recruitment to age 2 
  hatch_yr2 <- second_yr - (max_age2-2)
  hatch_yr3 <- third_yr - (max_age3-2)
  earliest_yr <- min(c(hatch_yr1, hatch_yr2, hatch_yr3))
  
  find_min$earliest_yr[which(find_min$name==i)] <- earliest_yr
}

find_min <- find_min %>%
  select(name, earliest_yr)
find_min <- unique(find_min)

out <- left_join(out, find_min, by=c("name"))

out <- out %>% group_by(name) %>%
  mutate(med = ifelse(year < earliest_yr, NA, med))

fit$labels[fit$order]

#get cluster order
name_order <- fit$labels[fit$order]

out$name <- factor(out$name, levels=name_order)

#Get the right colours
brewer.pal(8, name="Dark2")
display.brewer.pal(8, name="Dark2")

out$cluster

#7 == "#1B9E77" #Lac Bellevue
#6 == "#D95F02" #touchwood
#5 == "#66A61E" #Winagami
#4 == "#666666" #calling, wadlin
#3 == "#A6761D" #ioesegun
#2 == "#E7298A" #orloff
#1 == "#7570B3" # LSA
#8 == "#E6AB02" # Sturgeon lake

my_cols <- c("#7570B3", "#E7298A", 
             "#A6761D", "#666666",
             "#66A61E", "#D95F02", 
             "#1B9E77", "#E6AB02")

out$my_colors <- my_cols[out$cluster]
out <- out[order(out$name), ]

colors <- out[order(out$name),c("my_colors", "name")]

colors <- colors %>% group_by(name) %>%
  summarize(my_color = unique(my_colors))

p <- out %>%
  ggplot(aes(x = year, y=name,
             height = med)) + # ^(1 / 4)
  geom_ridgeline(fill = NA, size = 0.5, scale = 0.035) +
  ylab("") +
  xlab("") +
  scale_x_continuous(
    breaks = c(1984, 1990, 2000, 2009, 2018),
    limits = c(1984, 2019),
    expand = c(0, 0)
  ) +
  ggsidekick::theme_sleek() +
  theme(
    legend.position = "none",
    axis.line = element_blank(),
    axis.title = element_text(size = 12, colour = "grey30"),
    strip.text.x = element_text(size = 8, colour = "grey30"),
    panel.spacing.x = unit(0, "lines"),
    panel.spacing.y = unit(0, "lines"),
    panel.grid = element_blank(),
    axis.ticks.x = element_blank(),
    axis.ticks.y = element_blank(),
    panel.border = element_blank(),
    axis.text.x = element_text(
      angle = 90, size = 8, vjust = 0.45,
      margin = margin(-10, 0, 0, 0)
    ),
    axis.text.y = element_text(size = 8, vjust = -1, hjust = 1, 
                               colour = colors$my_color),
    plot.title = element_text(size = 8, hjust = 0.5)
  )
p 

ggsave("plots/ridgelines_cluster_notransform.png",
       width = 5,
       height = 8,
       dpi = 2000
)

ggsave("plots/ridgelines_cluster_notransform.pdf",
       width = 5,
       height = 8
)

