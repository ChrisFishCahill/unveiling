#--------------------------------------------------------
#Read in the data, declare some variables
data <- readRDS("data/BERTA-wide-0-25.rds")
stocking <- readRDS("data/stocking_matrix_ha.rds")

Ages <- 2:20
t <- 2000 # first fwin survey year
max_a <- max(Ages)
rec_a <- min(Ages)
initial_yr <- t - max_a + rec_a - 2 
add_year <- initial_yr - 1

n_iter = 2000
n_chains = 10
n_warmup = n_iter/2

names <- unique(data$name)
#--------------------------------------------------------
which_lakes <- c("buck lake", "amisk lake")

#Fit models
run_data <- data %>% filter(name %in% which_lakes)

run_data <-
  within(run_data, lake <-
           as.numeric(interaction(
             run_data$WBID,
             drop = TRUE, lex.order = F
           )))
run_data <- run_data[order(run_data$lake), ]

#stocking stuff was run, now just for plotting: 
run_stocking <- stocking[which(rownames(stocking) %in% which_lakes), ]
# Add ten years of zero for short term projections
proj_stock <- matrix(0, nrow = nrow(run_stocking), ncol = 10) # ten year projection
run_stocking <- round(cbind(run_stocking, proj_stock))

run_data <-
  within(run_data, lake <-
           as.numeric(interaction(
             run_data$WBID,
             drop = TRUE, lex.order = F
           )))
run_data <- run_data[order(run_data$lake), ]

# Set up the Rbar years
suppressMessages(
  survey_yrs <- run_data %>%
    group_by(lake) %>%
    summarise(
      min_yr = min(year) + length(initial_yr:(t - 1)),
      max_yr = max(year) + length(initial_yr:(t - 1))
    )
)
# summarize the life history relationships
suppressMessages(
  life_hist <- run_data %>%
    group_by(lake) %>%
    summarize(
      a50 = unique(a50),
      vbk = unique(vbk),
      linf = unique(linf),
      wl_beta = unique(beta_wl)
    )
)
#--------------------------------
stan_data <- list(
  n_surveys = nrow(run_data),
  n_ages = length(Ages),
  n_obs = nrow(run_data) * length(Ages),
  n_years = length(initial_yr:2028),
  n_lakes = length(unique(run_data$lake)),
  caa = run_data[, which(colnames(run_data) %in% Ages)],
  prop_aged = run_data$p_aged,
  effort = run_data$effort,
  lake = run_data$lake,
  year = run_data$year + length(initial_yr:(t - 1)),
  ages = Ages,
  survey_yrs = survey_yrs[, 2:3],
  which_year = 1996 - initial_yr + 2, # which integer corresponds to year = 1997
  v_prior_early = 0.3,
  v_prior_late = 0.1,
  prior_sigma_v = c(0.1, 0.5),
  R0_mean = log(6),
  R0_sd = log(3),
  ar_sd = 0.1,
  prior_mean_w = 0,
  prior_sigma_w = 1.2,
  vbk = life_hist$vbk,
  linf = life_hist$linf,
  a50 = life_hist$a50,
  wl_beta = life_hist$wl_beta,
  lbar = 57.57, #From cahill et al. 2020
  M = 0.1,
  theta = 0.85, #Lorenzen M exponent
  phi = 2.02,   #vulnerability parameter (nets)
  psi = 2,      #vulnerability parameter (angling)
  G_bound = c(0, Inf),
  get_SSB_obs = 1L,
  obs_cv_prior = 0.15,
  SSB_penalty = 0,
  prior_sigma_G = 1,
  length_Fseq = length(seq(from = 0.01, to = 1.0, by = 0.01)),
  Fseq = seq(from = 0.01, to = 1.0, by = 0.01),
  cr_prior = 6, 
  wcann = 0.1, 
  cpow = 0.67, 
  Mcann_mean = 0, 
  Mcann_sd = 5
)

# Start values
vk1 <- rep(0.3, length(unique(run_data$lake)))
vk2 <- rep(0.3, length(unique(run_data$lake)))
vk <- cbind(vk1, vk2)
inits <- function() {
  list(
    v = jitter(vk, amount=0.1),
    R0 = jitter(rep(15, stan_data$n_lakes), amount=2),
    #G = jitter(rep(1, stan_data$n_lakes), amount=0.1),
    w = jitter(matrix(0, nrow = stan_data$n_lakes, 
                      ncol = stan_data$n_years - 2), amount=0.1),
    sigma_w = jitter(0.5, amount=0.05), 
    ar = jitter(rep(1, stan_data$n_lakes), amount=0.01), 
    Mcann = rep(2, stan_data$n_lakes)
  )
}

# load .stan
m <- rstan::stan_model("stan-files/BERTA_unveiling_cannibalism.stan", verbose = F)

fit <-
  rstan::sampling(
    m,
    data = stan_data,
    pars =
      c(
        "Mcann", "cr", "ar", "br", 
        "Fmsy", "MSY", "F_ratio", "b_ratio",
        "cpro_kick", "canr", "C0", "Mcann", "cr", "ar", "br", 
        "ar_mean_kick", 
        "SPR",
        "SBR", "sbr0_kick", "R0", "v", "SSB",
        "R2", "SSB_obs", "caa_pred", "w"
      ),
    iter = 500,
    warmup = 250,
    chains = 1,
    init = inits,
    control = list(
      adapt_delta = 0.999,
      max_treedepth = 15
    )
  )

#########################################################
# make the relevant pairs plots
posterior <- as.array(fit)

#Let's play with buck: 
#rename crap to make it pretty
dimnames(posterior)$parameters[which(dimnames(posterior)$parameters=="v[2,2]")] = "Flate"
dimnames(posterior)$parameters[which(dimnames(posterior)$parameters=="v[2,1]")] = "Fearly"
dimnames(posterior)$parameters[which(dimnames(posterior)$parameters=="MSY[2]")] = "MSY"
dimnames(posterior)$parameters[which(dimnames(posterior)$parameters=='Fmsy[2]')] = "Fmsy"
dimnames(posterior)$parameters[which(dimnames(posterior)$parameters=='SPR[2]')] = "SPR"
dimnames(posterior)$parameters[which(dimnames(posterior)$parameters=='cr[2]')] = "CR"

dimnames(posterior)$parameters[which(dimnames(posterior)$parameters=='ar[2]')] = "ar"
dimnames(posterior)$parameters[which(dimnames(posterior)$parameters=='canr[2]')] = "canr"
dimnames(posterior)$parameters[which(dimnames(posterior)$parameters=='br[2]')] = "br"
dimnames(posterior)$parameters[which(dimnames(posterior)$parameters=='Mcann[2]')] = "Mcann"





np <- bayesplot::nuts_params(fit)

# library(bayesplot)
theme_set(bayesplot::theme_default() +
            theme(
              axis.text.x = element_text(size = 5.5),
              axis.text.y = element_text(size = 5.5)
            ))

# theme_set(my_theme)
pp <- bayesplot::mcmc_pairs(posterior,
                            pars = c(
                              'ar',
                              "br",
                              "canr",
                              "Mcann"
                            ),
                            off_diag_args = list(size = 1, alpha = 0.5),
                            np = np
)
pp

ggsave("plots/pp_can_tightw.pdf",
       plot=pp,
       width = 7,
       height = 7
)

#################################################
lake_name <- "buck lake"

id <- unique(run_data[which(run_data$name == lake_name), "lake"])

# set up the proper strings of misery
Fmsy <- "Fmsy[]"
MSY <- "MSY[]"
v <- "v[,2]"
v1 <- "v[,1]"
SPR <- "SPR[]"
SBR <- "SBR[]"
G <- "G[]"
cr <- "cr[]"
ar_mean <- "ar_mean_kick[]"

stringi::stri_sub(Fmsy, from = 6, to = 5) <- id
stringi::stri_sub(MSY, from = 5, to = 4) <- id
stringi::stri_sub(v, from = 3, to = 2) <- id
stringi::stri_sub(v1, from = 3, to = 2) <- id
stringi::stri_sub(SPR, from = 5, to = 4) <- id
stringi::stri_sub(SBR, from = 5, to = 4) <- id
stringi::stri_sub(G, from = 3, to = 2) <- id
stringi::stri_sub(cr, from = 4, to = 3) <- id
stringi::stri_sub(ar_mean, from = 14, to = 13) <- id

trajectory_dat <- fit %>%
  spread_draws(R2[lake, year]) %>%
  median_qi() %>%
  mutate(
    value = R2,
    year = year + add_year
  )

trajectory_dat$which <- "recruits"

SSB_dat <- fit %>%
  spread_draws(SSB[lake, year]) %>%
  median_qi() %>%
  mutate(
    value = SSB,
    year = year + add_year
  )

SSB_dat$which <- "female biomass"

trajectory_dat <- rbind(trajectory_dat, SSB_dat)

trajectory_dat$name <- NA
trajectory_dat$WBID <- NA
for (lake in 1:nrow(trajectory_dat)) {
  which_lake <- trajectory_dat$lake[lake]
  trajectory_dat$name[lake] <- tolower(run_data$name[which(run_data$lake == which_lake)][1])
  trajectory_dat$WBID[lake] <- run_data$WBID[which(run_data$lake == which_lake)][1]
}

SSB_C_dat <- fit %>%
  spread_draws(SSB_obs[Nobs]) %>%
  median_qi() %>%
  mutate(
    SSB_obs = SSB_obs,
    SSB_lower = SSB_obs - 0.20 * SSB_obs,
    SSB_upper = SSB_obs + 0.20 * SSB_obs
  )
SSB_C_dat$year <- stan_data$year + add_year
SSB_C_dat$name <- tolower(run_data$name)

trajectory_dat <- left_join(trajectory_dat, SSB_C_dat, 
                            by = c("name", "year"))

#--------------
#stocking = run_stocking
stock_surv <- 0.01
stock_dat <- data.frame(
  stock = run_stocking[which(rownames(run_stocking) == lake_name), ],
  year = initial_yr:2028
)

stock_dat <- stock_dat %>% mutate(stock = stock * stock_surv)
stock_dat$stock[stock_dat$stock == 0] <- NA
stock_dat$name <- lake_name

trajectory_dat <- left_join(trajectory_dat, stock_dat, 
                            by = c("name", "year"))

sbr0_dat <- fit %>%
  spread_draws(sbr0_kick[lake]) %>%
  median_qi() %>%
  mutate(sbr0 = sbr0_kick)

trajectory_dat <- left_join(trajectory_dat, sbr0_dat, by = "lake")

trajectory_dat <- trajectory_dat %>% filter(name == lake_name)

trajectory_dat <- trajectory_dat %>% filter(year > 1989)

trajectory_plot <- trajectory_dat %>%
  ggplot(aes(x = year, y = value, colour = factor(which))) +
  scale_color_manual(values = c("steelblue", "darkorange2")) +
  geom_line(lwd = 1) +
  geom_ribbon(aes(ymin = .lower.x, ymax = .upper.x),
              linetype = 2,
              alpha = 0.25
  ) +
  xlab("Year") +
  ylab("Female Biomass (kg/ha)") +
  xlim(1990, 2028) +
  scale_x_continuous(breaks = c(1990, 2000, 2010, 2020, 2028)) +
  ggsidekick::theme_sleek() +
  geom_point(aes(x = year, y = SSB_obs), colour = "black") +
  geom_linerange(aes(x = year, ymin = SSB_lower, ymax = SSB_upper),
                 colour = "black"
  ) +
  geom_point(aes(x = year + 2, y = stock),
             shape = 23, fill = "darkorange2",
             color = "steelblue", size = 3
  ) +
  scale_y_continuous(sec.axis = sec_axis(~ . * 1,
                                         name = "Age 2 Recruits (N/ha)"
  )) +
  labs(
    title = eval(lake_name),
    subtitle = paste0("waterbody ID: ", unique(trajectory_dat$WBID))
  ) +
  theme(
    axis.text.x = element_text(size = 8),
    strip.text.x = element_text(size = 8)
  ) +
  theme(legend.title = element_blank()) +
  theme(legend.position = c(0.15, 0.95)) +
  if (max(SSB_C_dat$SSB_upper) * 3 < max(trajectory_dat$.upper.x)) {
    coord_cartesian(ylim = c(0, 3 * max(SSB_C_dat$SSB_obs)))
  }
trajectory_plot

ggsave("plots/canibalism_trajectory_wide_w.pdf",
       plot=trajectory_plot,
       width = 7,
       height = 5
)

# trajectory_plot <- trajectory_plot +
#   annotate("text", x = 1984.8, y = max(trajectory_dat$.upper.x) * 0.9, 
#label = paste0("ssbc/rbar: ", 
#round(unique(trajectory_dat$sbr0), 2)))
which_x <- 1990
hjust <- 0
size <- 3
trajectory_plot <- trajectory_plot +
  annotate("text", which_x, Inf,
           vjust = 5.5, hjust = hjust,
           label = paste0("sbro: ", round(unique(trajectory_dat$sbr0), 2)),
           size = size
  )

trajectory_plot <- trajectory_plot +
  annotate("text", which_x, Inf,
           vjust = 6.75, hjust = hjust,
           label = paste0(
             "a50: ",
             stan_data$a50[unique(trajectory_dat$lake)]
           ),
           size = size
  )

#even more string misery
ar_string <- "ar ~ N(,)"
a_value <- format(round(rstan::extract(fit, pars = ar_mean)$`ar_mean`[1], 2), nsmall=2)

a_idx <- stringr::str_locate_all(string = ar_string, pattern = "\\(,")[[1]]
stringi::stri_sub(ar_string, from = a_idx[2], to = a_idx[1]) <- a_value

sd_idx <- stringr::str_locate_all(string = ar_string, pattern = ",\\)")[[1]]
stringi::stri_sub(ar_string, from = sd_idx[2], to = sd_idx[1]) <- stan_data$ar_sd
ar_string

trajectory_plot <- trajectory_plot +
  annotate("text", which_x, Inf,
           vjust = 8.0, hjust = hjust,
           label = ar_string, size = size
  )

