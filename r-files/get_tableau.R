# Abandon all hope, ye who enter here
# Make a tableau for each lake
library(cowplot)
library(bayesplot)
library(tidybayes)
library(tidyverse)
#------------------------
getLevel <- function(x, y, prob = 0.95) {
  kk <- MASS::kde2d(x, y)
  dx <- diff(kk$x[1:2])
  dy <- diff(kk$y[1:2])
  sz <- sort(kk$z)
  c1 <- cumsum(sz) * dx * dy
  approx(c1, sz, xout = 1 - prob)$y
}

get_tableau <- function(fit, stan_data, data, stocking) {
  for (i in unique(data$name)) {
    lake_name <- i

    id <- unique(data[which(data$name == lake_name), "lake"])
    
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
    
    filename <- "tableaus/tableau_.pdf"
    stringi::stri_sub(filename, from = 18, to = 17) <- lake_name
    filename <- gsub(" ", "_", filename)
    which_idx <- stringr::str_locate_all(pattern = ".pdf", filename)[[1]]
    # fix the daggum file name to something (hopefully) intelligible
    if (stan_data$rec_model == 0) {
      stringi::stri_sub(filename, from = which_idx[1], 
                        to = which_idx[1] - 4) <- "_ricker"
    }
    if (stan_data$rec_model == 1) {
      stringi::stri_sub(filename, from = which_idx[1], 
                        to = which_idx[1] - 4) <- "_bh"
    }
    if (stan_data$cr_prior == 6) {
      which_idx <- stringr::str_locate_all(pattern = ".pdf", filename)[[1]]
      stringi::stri_sub(filename, from = which_idx[1], 
                        to = which_idx[1] - 8) <- "_cr6"
    }
    if (stan_data$cr_prior == 12) {
      which_idx <- stringr::str_locate_all(pattern = ".pdf", filename)[[1]]
      stringi::stri_sub(filename, from = which_idx[1], 
                        to = which_idx[1] - 8) <- "_cr12"
    }
    # file name for trajectory pop up
    filename2 <- stringr::str_replace(filename, "tableaus", "popups")
    filename2 <- stringr::str_replace(filename2, "tableau", "trajectory")
    filename3 <- stringr::str_replace(filename2, ".pdf", ".png")
    
    #################################################
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
      trajectory_dat$name[lake] <- tolower(data$name[which(data$lake == which_lake)][1])
      trajectory_dat$WBID[lake] <- data$WBID[which(data$lake == which_lake)][1]
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
    SSB_C_dat$name <- tolower(data$name)
    
    trajectory_dat <- left_join(trajectory_dat, SSB_C_dat, 
                                by = c("name", "year"))
    
    #--------------
    #stocking = run_stocking
    stock_surv <- 0.01
    stock_dat <- data.frame(
      stock = stocking[which(rownames(stocking) == lake_name), ],
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
    
    # Extra code for when geezer and I were adding more to plots / troubleshooting
    # Fearly_string <- "Fearly ~ N(,)"
    # stringi::stri_sub(Fearly_string, from = 12, 
    #                   to = 11) <- stan_data$v_prior_early
    # stringi::stri_sub(Fearly_string, from = 16, 
    #                   to = 15) <- stan_data$prior_sigma_v[1]
    # 
    # trajectory_plot <- trajectory_plot +
    #   annotate("text", which_x, Inf,
    #            vjust = 9.25, hjust = hjust,
    #            label = Fearly_string, size = size
    #   )
    # 
    # Flate_string <- "Flate ~ N(,)"
    # stringi::stri_sub(Flate_string, from = 11, 
    #                   to = 10) <- stan_data$v_prior_late
    # stringi::stri_sub(Flate_string, from = 15, 
    #                   to = 14) <- stan_data$prior_sigma_v[2]
    # 
    # trajectory_plot <- trajectory_plot +
    #   annotate("text", which_x, Inf,
    #            vjust = 10.5, hjust = hjust,
    #            label = Flate_string, size = size
    #   )
    # 
    # w_string <- "w ~ N(0,)"
    # stringi::stri_sub(w_string, from = 9, to = 8) <- stan_data$prior_sigma_w
    # 
    # trajectory_plot <- trajectory_plot +
    #   annotate("text", which_x, Inf,
    #            vjust = 11.75, hjust = hjust,
    #            label = w_string, size = size
    #   )
    # 
    # G_string <- "G ~ N(0,)"
    # stringi::stri_sub(G_string, from = 9, to = 8) <- stan_data$prior_sigma_G
    # 
    # trajectory_plot <- trajectory_plot +
    #   annotate("text", which_x, Inf,
    #            vjust = 13.0, hjust = hjust,
    #            label = G_string, size = size
    #   )
    
    #Save a .pdf
    ggsave(filename2,
           width = 6,
           height = 4
    )
    
    #save a .png 
    ggsave(filename3,
           width = 6,
           height = 4
    )
    
    #-----------------------
    # make the relevant pairs plots
    posterior <- as.array(fit)
    
    #rename crap to make it pretty
    dimnames(posterior)$parameters[which(dimnames(posterior)$parameters==v)] = "Flate"
    dimnames(posterior)$parameters[which(dimnames(posterior)$parameters==v1)] = "Fearly"
    dimnames(posterior)$parameters[which(dimnames(posterior)$parameters==MSY)] = "MSY"
    dimnames(posterior)$parameters[which(dimnames(posterior)$parameters==Fmsy)] = "Fmsy"
    dimnames(posterior)$parameters[which(dimnames(posterior)$parameters==SPR)] = "SPR"
    dimnames(posterior)$parameters[which(dimnames(posterior)$parameters==cr)] = "CR"
    
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
                                  'Fmsy',
                                  "MSY",
                                  "Fearly",
                                  "Flate"
                                ),
                                off_diag_args = list(size = 1, alpha = 0.5),
                                np = np
    )
    
    my_theme <- theme(axis.text = element_text(size = 5.5))
    # pp + theme(text = element_text(size=20))
    
    pp_hist <- bayesplot::mcmc_hist(posterior, pars = "SPR") +
      ggsidekick::theme_sleek() + my_theme
    
    # pp_hist2 <- bayesplot::mcmc_hist(posterior, pars = SBR) + my_theme
    # pp_hist3 <- bayesplot::mcmc_hist(posterior, pars = G) + my_theme
    pp_hist2 <- bayesplot::mcmc_hist(posterior, pars = "CR") +
      ggsidekick::theme_sleek() + my_theme
    
    #------------------------
    # make an observed vs. predicted plot
    plot_dat <- fit %>%
      spread_draws(caa_pred[n_obs]) %>%
      median_qi()
    
    plot_dat$age <- NA
    plot_dat$age <- rep(Ages, nrow(plot_dat) / length(Ages))
    
    my_dat <- data[, which(colnames(data) %in% Ages)]
    my_dat$year <- data$year
    my_dat$name <- data$name
    
    my_dat <- pivot_longer(my_dat,
                           cols = which(colnames(my_dat) %in% Ages),
                           names_to = "age", names_ptype = integer(),
                           values_to = "caa"
    )
    my_dat$which <- NA
    my_dat$which <- "observed"
    
    plot_dat$caa_obs <- NA
    plot_dat$name <- NA
    plot_dat$year <- NA
    plot_dat$caa_obs <- my_dat$caa_obs
    plot_dat$name <- my_dat$name
    plot_dat$year <- my_dat$year
    
    plot_dat <- plot_dat %>% dplyr::select(year, name, age, caa_pred)
    plot_dat$which <- "predicted"
    names(plot_dat)[4] <- "caa"
    
    plot_dat <- rbind(plot_dat, my_dat)
    plot_dat$string <- paste(plot_dat$name, plot_dat$year, sep = " ")
    plot_dat <- plot_dat %>% mutate(year = year + 1999)
    plot_dat$age <- as.numeric(plot_dat$age)
    
    plot_dat <- plot_dat %>% filter(name == lake_name)
    # plot_dat <- plot_dat %>% filter(year==2003)
    # plot_dat <- tidyr::complete(plot_dat, fill=list(year=NA))
    # how_many <- length(unique(plot_dat$year))
    # if(how_many<13){
    #  fake_fct <- 1:(13 - length(unique(plot_dat$year))) + 2018
    #  plot_dat <- plot_dat %>% add_row(year = fake_fct)
    # }
    
    obs_v_pred <- plot_dat %>%
      ggplot(aes(x = age, y = caa, colour = factor(which))) +
      geom_point(pch = 16, size = 2) +
      scale_color_manual(
        values = c("steelblue", "darkorange2"),
        na.translate = F
      ) +
      geom_line(data = subset(plot_dat, which == "predicted")) +
      xlab("Age") +
      ylab("Catch") +
      ggsidekick::theme_sleek() +
      theme(legend.title = element_blank()) +
      facet_wrap(~year, ncol = 1, nrow = 13, scales = "free_y") +
      theme(
        legend.position = c(0.799, 0.99),
        legend.text = element_text(size = 5.5),
        legend.key.size = unit(0.25, "lines"),
        axis.text.y = element_text(size = 5.5),
        axis.text.x = element_text(size = 5.5),
        strip.text = element_text(size = 5.5, vjust = -1.5),
        axis.title.y = element_text(size = 6),
        axis.title.x = element_text(size = 6),
      )
    
    # Bubble plots
    sub_dat <- data %>% filter(name == lake_name)
    yr_dat <- sub_dat$year
    sub_dat <- sub_dat[, c(which(colnames(sub_dat) %in% 2:25))]
    sub_dat$year <- yr_dat
    sub_dat <- sub_dat %>%
      pivot_longer(-year, names_to = "age") %>%
      mutate(year = year + 1999)
    
    sub_dat$age <- as.integer(sub_dat$age)
    sub_dat$value[sub_dat$value == 0] <- NA
    bp <- ggplot(sub_dat, aes(x = year, y = age, size = value)) +
      geom_point(alpha = 0.75, colour = "darkorange2") +
      scale_size(range = c(.1, 3), name = "Number of Fish") +
      ggsidekick::theme_sleek() +
      scale_x_continuous(breaks = seq(2000, 2018, 1)) +
      scale_y_continuous(breaks = c(2, 5, 10, 15, 20), limits = c(2, 20)) +
      expand_limits(x = c(2000, 2018)) +
      ylab("Age") +
      xlab("Year") +
      theme(
        legend.position = "none",
        axis.title.y = element_text(size = 6),
        axis.title.x = element_text(size = 6),
        axis.text.x = element_text(
          angle = 90, size = 4.5,
          hjust = 0.95, vjust = 0.2
        ),
        axis.text.y = element_text(size = 5.5)
        # legend.text = element_text(size = 1),
        # legend.key.size = unit(0.25, 'lines'),
        # legend.direction = "horizontal",
        # axis.text.x = element_text(angle = 90, size=5.5)
      )
    # bp
    
    #---------
    # egglette
    trajectory_dat <- fit %>%
      spread_draws(b_ratio[lake], F_ratio[lake])
    
    trajectory_dat$name <- NA
    trajectory_dat$WBID <- NA
    for (lake in 1:nrow(trajectory_dat)) {
      which_lake <- trajectory_dat$lake[lake]
      trajectory_dat$name[lake] <- tolower(data$name[which(data$lake == which_lake)][1])
      trajectory_dat$WBID[lake] <- data$WBID[which(data$lake == which_lake)][1]
    }
    
    my_dat <- trajectory_dat %>%
      filter(name == lake_name)
    L90 <- getLevel(my_dat$b_ratio, my_dat$F_ratio, prob = 0.90)
    L75 <- getLevel(my_dat$b_ratio, my_dat$F_ratio, prob = 0.75)
    L50 <- getLevel(my_dat$b_ratio, my_dat$F_ratio, prob = 0.50)
    
    if (max(my_dat$b_ratio) < 2) {
      xmax <- 2
    } else {
      xmax <- max(my_dat$b_ratio) + 0.25
    }
    
    if (max(my_dat$F_ratio) < 2) {
      ymax <- 2
    } else {
      ymax <- max(my_dat$F_ratio) + 0.25
    }
    
    egglette <- my_dat %>%
      ggplot(aes(x = b_ratio, y = F_ratio)) +
      xlim(0, xmax) +
      ylim(0, ymax) +
      stat_density2d(
        colour = "gray", contour = T, geom = "polygon",
        fill = "snow", breaks = L90, size = 0.75
      ) +
      # stat_density2d(colour="darkorange2",breaks=L75, size=1.2)+
      stat_density2d(
        colour = "darkorange2", geom = "polygon", breaks = L50,
        fill = "darkorange2", size = 0.75
      ) +
      geom_point(pch = 16, size = 0.01, alpha = 0.2) +
      ggsidekick::theme_sleek() +
      ylab("F / Fmsy") +
      xlab("Mean Survey SSB / SSBo") +
      geom_hline(yintercept = 1, lty = 1, size = 0.75, alpha = 0.25) +
      geom_vline(xintercept = 0.4, lty = 1, size = 0.75, alpha = 0.25) +
      theme(
        axis.text.x = element_text(size = 5.5),
        axis.text.y = element_text(size = 5.5),
        axis.title = element_text(size = 7.5)
      )
    
    # egglette
    
    #---------
    # Make the tableau
    bottom_left_hists <- cowplot::plot_grid(egglette, pp_hist,
                                            pp_hist2,
                                            nrow = 1
    )
    
    left_column <- cowplot::plot_grid(trajectory_plot, bottom_left_hists, pp,
                                      ncol = 1,
                                      rel_heights = c(0.33, 0.15, 0.33)
    )
    
    right_column <- cowplot::plot_grid(bp, NULL, obs_v_pred,
                                       ncol = 1,
                                       rel_heights = c(0.2, -0.01, 1)
    )
    
    my_plot <- cowplot::plot_grid(left_column, right_column,
                                  rel_widths = c(1, 0.35)
    )
    ggsave(filename,
           width = 8,
           height = 11
    )
  }
}
