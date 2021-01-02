# estimate fwin survey vulnerability

#starting values
phi <- 2 # survey vulnerability 
psi <- 2 # Mpow
N0 <- 65
F0 <- 0.17

vbk_bar <- 0.23
ages <- 1:20
M0 <- 0.1
ahv <- 6
sdv <- 1

data <- readRDS("data/BERTA-wide-0-25.rds")

caa <- data[, which(colnames(data) %in% ages)]

nnet_bar <- mean(data$nnet)

total_caa <- caa %>%
  pivot_longer(
    cols = ages, names_to = "age",
    names_transform = list(age = as.numeric),
    values_to = "count"
  ) %>%
  group_by(age) %>%
  summarize(count = sum(count))

caa_obs2 <- total_caa$count / (105*nnet_bar)

get_negloglike <- function(pars){
  F0 <- pars[1]
  N0 <- pars[2]
  phi <- pars[3]
  psi <- pars[4]
  
  Ma <- M0 / (1-exp(-vbk_bar*ages))^psi
  Fa <- F0*((1-exp(-vbk_bar*ages)) / (1+exp(-(ages-ahv)/sdv)))

  Za <- Ma + Fa

  Na <- rep(NA, length(ages))
  Na[1] <- N0
  for(i in 2:length(ages)){
   Na[i] = Na[i-1]*exp(-Za[i-1])
  }

  va <- rep(NA, length(ages))
  va <- (1-exp(-vbk_bar*ages))^phi
  cpred_a <- Na*va
  
  nll <- +sum(cpred_a) - sum(caa_obs*log(cpred_a))
  nll 
}
  
pars <- c(0.17, 65, 2, 2) #F0, N0, phi (vpow), psi (mpow)
get_negloglike(pars)

fit <- nlminb(pars, get_negloglike, lower=0, upper=Inf)
fit2 <- nlminb(fit$par, get_negloglike, lower=0, upper=Inf) 
fit$objective
fit2$objective

F0_mle <- fit2$par[1]
N0_mle <- fit2$par[2]
vpow_mle <- fit2$par[3]
mpow_mle <- fit2$par[4]

v_a <- (1-exp(-vbk_bar*ages))^vpow_mle 
F_a <- F0_mle*((1-exp(-vbk_bar*ages)) / (1+exp(-(ages-ahv)/sdv)))
caa_obs 

Ma <- M0 / (1-exp(-vbk_bar*ages))^mpow_mle
Za <- Ma + F_a

Na <- rep(NA, length(ages))
Na[1] <- N0_mle
for(i in 2:length(ages)){
  Na[i] = Na[i-1]*exp(-Za[i-1])
}

cpred_a <- Na*v_a  

data <- data.frame(
  age = ages, caa_pred = v_a*Na, 
  caa_obs = caa_obs, Naa = Na, 
  app_vage = caa_obs/Na, 
  v_a = v_a
)

p1 <- data %>%
  ggplot(aes(x = age, y = caa_pred)) +
  geom_point(size = 1, colour = "steelblue") +
  geom_line(colour = "steelblue") +
  geom_point(aes(x = age, y = caa_obs), colour = "darkorange2") +
  ylab("Average catch per net") +
  xlab("Age") +
  expand_limits(x = c(1, 20)) +
  scale_x_continuous(breaks = c(1, 5, 10, 15, 20), limits = c(1, 20)) +
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

p1

p2 <- data %>%
  ggplot(aes(x = age, y = F_a)) +
  geom_point(size = 1, colour = "steelblue") +
  geom_line(colour = "steelblue") +
  ylab("Instantaneous fishing mortality") +
  xlab("Age") +
  expand_limits(x = c(1, 20)) +
  scale_x_continuous(breaks = c(1, 5, 10, 15, 20), limits = c(1, 20)) +
  scale_y_continuous(limits = c(0.00, 0.20)) +
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

p2

p3 <- data %>%
  ggplot(aes(x = age, y = v_a)) +
  geom_point(size = 1, colour = "steelblue") +
  geom_line(colour = "steelblue") +
  geom_point(aes(x = age, y = app_vage), colour = "darkorange2") +
  geom_line(aes(x = age, y = app_vage), colour = "darkorange2") +
  ylab("Vulnerability at age") +
  xlab("Age") +
  expand_limits(x = c(1, 20)) +
  scale_x_continuous(breaks = c(1, 5, 10, 15, 20), limits = c(1, 20)) +
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

p3

p4 <- data %>%
  ggplot(aes(x = age, y = Naa)) +
  geom_point(size = 1, colour = "steelblue") +
  geom_line(colour = "steelblue") +
  geom_point(aes(x = age, y = caa_obs), colour = "darkorange2") +
  ylab("Catch or numbers at age") +
  xlab("Age") +
  expand_limits(x = c(1, 20)) +
  scale_x_continuous(breaks = c(1, 5, 10, 15, 20), limits = c(1, 20)) +
  scale_y_continuous(trans = "log10") +
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

p4

S2plot <- cowplot::plot_grid(p1, p2, p3, p4,
                             nrow = 2, 
                             rel_widths = c(0.5,0.5)
)


#
ggsave("plots/S2plot.pdf",
  width = 6,
  height = 6
)

ggsave("plots/S2plot.png",
       width = 6,
       height = 6, 
       dpi=2000
)

#a few extra checks
# linf <- 57.57
# t0 <- -0.95
# vbk_bar
# ages = 1:20
# 
# l_a <- linf*(1-exp(-vbk_bar*(ages-t0)))
# 
# data$l_a <- l_a*10
# 
# plot(data$v_a~data$l_a, ylab="v(a)", xlab="length(mm)", 
#      xlim=c(100, 600), type="b")


 