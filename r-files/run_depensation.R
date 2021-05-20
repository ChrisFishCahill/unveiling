######################################################
#This script differs from others in that it runs depensatory ricker models, 
#as per turbo nerd emails with holly et al.
#
# Functions and code to call stan runs, save them, and make tableaus for each 
# run. Note that all files are saved on an external hard drive 
# (i.e., they are big) and thus are not in the unveiling directory 
# Also: this file sources get_tableau.R
# library(dplyr)
# library(stringr)
# library(strini)
source("r-files/get_tableau.R")
library(tidyverse)
options(mc.cores = parallel::detectCores())
rstan::rstan_options(auto_write = TRUE)

#function to iterate through lakes
get_fits <- function(rec_model = c("bev-holt", "ricker"), 
                     cr_prior = c(6, 12),
                     n_iter = n_iter, n_chains = n_chains, 
                     n_warmup = n_iter/2,
                     data = data, 
                     stocking = stocking,
                     v_f_ctl = 0, ...) {
  rec_model <- match.arg(rec_model)
  counter <- 1:2 #run lakes in twos for speed
  which_fit <- 1

  for (i in 1:((length(names) / 2)+1)) {
    if(i==28){ #accounting to fit last lake (55/2 is not an even number) 
      counter=c(54,55)
    }
    cat(
      crayon::green(
        clisymbols::symbol$tick
      ),
      fitted = "recruitment model fitted = ", rec_model,
      "  lakes:", names[counter],
      sep = " "
    )
    cat("\n")
    which_lakes <- names[counter]
    
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
      Rinit_ctl = 0,
      length_Fseq = length(seq(from = 0.01, to = 1.0, by = 0.001)),
      Fseq = seq(from = 0.01, to = 1.0, by = 0.001),
      rec_model = ifelse(rec_model == "ricker", 0, 1),
      cr_prior = cr_prior, 
      v_f_ctl = v_f_ctl, 
      a_inflect = 3.5,
      delta = 2,
      m_prior = 0.5
    )
    
    # Start values
    vk1 <- rep(0.3, length(unique(run_data$lake)))
    vk2 <- rep(0.3, length(unique(run_data$lake)))
    vk <- cbind(vk1, vk2)
    inits <- function() {
      list(
        v = jitter(vk, amount=0.1),
        R0 = jitter(rep(15, stan_data$n_lakes), amount=2),
        G = jitter(rep(1, stan_data$n_lakes), amount=0.1),
        w = jitter(matrix(0, nrow = stan_data$n_lakes, 
                          ncol = stan_data$n_years - 2), amount=0.1),
        sigma_w = jitter(0.5, amount=0.05), 
        ar = jitter(rep(0.5, stan_data$n_lakes), amount=0.01),
        m = rep(1, stan_data$n_lakes)
      )
    }
    fit <-
      rstan::sampling(
        m,
        data = stan_data,
        pars =
          c(
            "m", "ar_mean_kick", "F_ratio", "Fmsy", "MSY",
            "G", "cr", "ar", "SPR", "br",
            "SBR", "sbr0_kick", "R0", "v", "SSB",
            "R2", "SSB_obs", "caa_pred", "b_ratio", "w"
          ),
        iter = n_iter,
        warmup = n_warmup,
        chains = n_chains,
        init = inits,
        control = list(
          adapt_delta = 0.999,
          max_treedepth = 15
        )
      )
    
    #Make a filename to fit on my external drive: 
    if (rec_model == "ricker") {
      stan_file <- paste0("D:/unveiling_fits/ricker//fit_", which_fit, ".rds")
    }
    
    if (stan_data$cr_prior == 6) {
      which_idx <- stringr::str_locate_all(pattern = "//", stan_file)[[1]]
      stringi::stri_sub(stan_file, from = which_idx[1]+1,
                        to = which_idx[2]-1) <- "depensation"
    }
    
    if (file.exists(stan_file)) {
      return(NULL)
    } else {
      saveRDS(fit, file = stan_file)
    }
    counter <- counter + 2
    which_fit <- which_fit + 1
  }
}

#Read in the data, declare some variables
data <- readRDS("data/BERTA-wide-0-25.rds")
stocking <- readRDS("data/stocking_matrix_ha.rds")

Ages <- 2:20
t <- 2000 # first fwin survey year
max_a <- max(Ages)
rec_a <- min(Ages)
initial_yr <- t - max_a + rec_a - 2 
add_year <- initial_yr - 1

# load .stan
m <- rstan::stan_model("stan-files/unveiling_depensation.stan", verbose = F)

n_iter = 2000
n_chains = 10
n_warmup = n_iter/2
names <- unique(data$name)

#--------------------------------------------------------
#Fit models

get_fits(rec_model = "ricker", cr_prior = 6,
         n_iter = n_iter, n_chains = n_chains,
         n_warmup = n_warmup,
         data=data, stocking=stocking)

#--------------------------------------------------------



##################################################
#Extra code I used to debug this stuff...
#
# Functions and code to call stan runs, save them, and make tableaus for each 
# run. Note that all files are saved on an external hard drive 
# (i.e., they are big) and thus are not in the unveiling directory 
# Also: this file sources get_tableau.R
# library(dplyr)
# library(stringr)
# library(strini)
# source("r-files/get_tableau.R")
# library(tidyverse)
# options(mc.cores = parallel::detectCores())
# rstan::rstan_options(auto_write = TRUE)
# 
# #Read in the data, declare some variables
# data <- readRDS("data/BERTA-wide-0-25.rds")
# stocking <- readRDS("data/stocking_matrix_ha.rds")
# 
# Ages <- 2:20
# t <- 2000 # first fwin survey year
# max_a <- max(Ages)
# rec_a <- min(Ages)
# initial_yr <- t - max_a + rec_a - 2 
# add_year <- initial_yr - 1
# 
# # load .stan
# m <- rstan::stan_model("stan-files/unveiling_depensation.stan", verbose = F)
# 
# names <- unique(data$name)
# 
# which_lakes <- c("lac ste. anne", "pigeon lake")
# 
# run_data <- data %>% filter(name %in% which_lakes)
# run_data <-
#   within(run_data, lake <-
#            as.numeric(interaction(
#              run_data$WBID,
#              drop = TRUE, lex.order = F
#            )))
# run_data <- run_data[order(run_data$lake), ]
# run_stocking <- stocking[which(rownames(stocking) %in% which_lakes), ]
# # Add ten years of zero for short term projections
# proj_stock <- matrix(0, nrow = nrow(run_stocking), ncol = 10) # ten year projection
# run_stocking <- round(cbind(run_stocking, proj_stock))
# 
# run_data <-
#   within(run_data, lake <-
#            as.numeric(interaction(
#              run_data$WBID,
#              drop = TRUE, lex.order = F
#            )))
# run_data <- run_data[order(run_data$lake), ]
# 
# # Set up the Rbar years
# suppressMessages(
#   survey_yrs <- run_data %>%
#     group_by(lake) %>%
#     summarise(
#       min_yr = min(year) + length(initial_yr:(t - 1)),
#       max_yr = max(year) + length(initial_yr:(t - 1))
#     )
# )
# # summarize the life history relationships
# suppressMessages(
#   life_hist <- run_data %>%
#     group_by(lake) %>%
#     summarize(
#       a50 = unique(a50),
#       vbk = unique(vbk),
#       linf = unique(linf),
#       wl_beta = unique(beta_wl)
#     )
# )
# #--------------------------------
# rec_model <- "ricker"
# cr_prior <- 6
# v_f_ctl = 0
# 
# stan_data <- list(
#   n_surveys = nrow(run_data),
#   n_ages = length(Ages),
#   n_obs = nrow(run_data) * length(Ages),
#   n_years = length(initial_yr:2028),
#   n_lakes = length(unique(run_data$lake)),
#   caa = run_data[, which(colnames(run_data) %in% Ages)],
#   prop_aged = run_data$p_aged,
#   effort = run_data$effort,
#   lake = run_data$lake,
#   year = run_data$year + length(initial_yr:(t - 1)),
#   ages = Ages,
#   survey_yrs = survey_yrs[, 2:3],
#   which_year = 1996 - initial_yr + 2, # which integer corresponds to year = 1997
#   v_prior_early = 0.3,
#   v_prior_late = 0.1,
#   prior_sigma_v = c(0.1, 0.5),
#   R0_mean = log(6),
#   R0_sd = log(3),
#   ar_sd = 0.1,
#   prior_mean_w = 0,
#   prior_sigma_w = 1.2,
#   vbk = life_hist$vbk,
#   linf = life_hist$linf,
#   a50 = life_hist$a50,
#   wl_beta = life_hist$wl_beta,
#   lbar = 57.57, #From cahill et al. 2020
#   M = 0.1,
#   theta = 0.85, #Lorenzen M exponent
#   phi = 2.02,   #vulnerability parameter (nets)
#   psi = 2,      #vulnerability parameter (angling)
#   G_bound = c(0, Inf),
#   get_SSB_obs = 1L,
#   obs_cv_prior = 0.15,
#   SSB_penalty = 0,
#   prior_sigma_G = 1,
#   Rinit_ctl = 0,
#   length_Fseq = length(seq(from = 0.01, to = 1.0, by = 0.001)),
#   Fseq = seq(from = 0.01, to = 1.0, by = 0.001),
#   rec_model = ifelse(rec_model == "ricker", 0, 1),
#   cr_prior = cr_prior, 
#   v_f_ctl = v_f_ctl, 
#   a_inflect = 3.5,
#   delta = 2, 
#   m_prior = 0.5
# )
# 
# # Start values
# vk1 <- rep(0.3, length(unique(run_data$lake)))
# vk2 <- rep(0.3, length(unique(run_data$lake)))
# vk <- cbind(vk1, vk2)
# inits <- function() {
#   list(
#     v = jitter(vk, amount=0.1),
#     R0 = jitter(rep(15, stan_data$n_lakes), amount=2),
#     G = jitter(rep(1, stan_data$n_lakes), amount=0.1),
#     w = jitter(matrix(0, nrow = stan_data$n_lakes, 
#                       ncol = stan_data$n_years - 2), amount=0.1),
#     sigma_w = jitter(0.5, amount=0.05), 
#     ar = jitter(rep(0.5, stan_data$n_lakes), amount=0.01), 
#     m = rep(1, stan_data$n_lakes)
#   )
# }
# 
# n_iter = 2000
# n_chains = 5
# n_warmup = n_iter/2
# 
# fit <-
#   rstan::sampling(
#     m,
#     data = stan_data,
#     pars =
#       c(
#         "m", "ar_mean_kick", "F_ratio", "Fmsy", "MSY",
#         "G", "cr", "ar", "SPR", "br",
#         "SBR", "sbr0_kick", "R0", "v", "SSB",
#         "R2", "SSB_obs", "caa_pred", "b_ratio", "w"
#       ),
#     iter = n_iter,
#     warmup = n_warmup,
#     chains = n_chains,
#     init = inits,
#     control = list(
#       adapt_delta = 0.999,
#       max_treedepth = 15
#     )
#   )



