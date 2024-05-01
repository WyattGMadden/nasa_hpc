library(tidyverse)
library(knitr)
library(kableExtra)
library(maps)
theme_set(theme_bw())
save_dir <- "../../output/figures/"
locs <- read.csv("../../data/howard_uploaded/CA_OR_MAIAC_Grid_wLatLon.csv", check.names = F)
names(locs) <- c("oid", tolower(names(locs)[2:ncol(locs)]))

obs <- readRDS("../../data/created/obs.rds")
obs <- merge(obs, 
             locs[, c("longitude", "latitude", "maiac_id")],
             by.x = "maiac_id", 
             by.y = "maiac_id", 
             all.x = TRUE)

preds <- readRDS("../../data/created/preds.rds")
preds <- merge(preds, 
               locs[, c("longitude", "latitude", "maiac_id")],
               by.x = "maiac_id", 
               by.y = "maiac_id", 
               all.x = TRUE)

output_files <- list.files("../../output/results/fits", full.names = T)
output_files <- output_files[grepl(".RDS", output_files)]

output_fit_files <- output_files[grepl("fit_", output_files)]
output_cv_files <- output_files[grepl("cv_", output_files)]

output_fits <- lapply(output_fit_files, readRDS)
output_cvs <- lapply(output_cv_files, readRDS)

length(output_cv_files)


cv_details <- readRDS("../../data/created/cv_objects/spatial.rds")
table(cv_details$cv.id)



kable <- function(x, digits = 2, ...) {
    x |>
    knitr::kable("latex", 
                 booktabs = T, 
                 align = "c", 
                 digits = digits,
                 linesep = "\\\\[-3.0ex]",
                 ...)
}

cv_out <- lapply(output_cvs, 
                 function(x) {
                     cv_fit <- x$ctm_fit_cv 
                     cv_fit <- cv_fit[!is.na(cv_fit$estimate), ]
                     cv_fit$matern_nu <- x$matern.nu
                     cv_fit$cv_type <- x$cv
                     cv_fit$time_fit_cv <- x$time_fit_cv['elapsed'] / 60^2
                     return(cv_fit)
                    }
                 ) |>
    (\(.) Reduce(rbind, .))() |>
    left_join(unique(obs[, c('time_id', 'space_id', 'x', 'y', 'date')]), 
              by = c('time_id', 'space_id')) |>
    mutate(buff_size = stringr::str_extract(cv_type, "\\d+"),
           buff_size = as.integer(buff_size),
           buff_size = if_else(is.na(buff_size), "", as.character(buff_size)),
           cv_type_spec = substr(cv_type, 1, 9),
           cv_type_spec = case_when(
               cv_type_spec == 'ordinary' ~ 'Ordinary',
               cv_type_spec == 'spatial' ~ 'Spatial',
               cv_type_spec == 'spatial_c' ~ 'Spatial Clustered',
               cv_type_spec == 'spatial_b' ~ 'Spatial Buffered'),
           cv_type_spec = if_else(cv_type_spec == 'Spatial Buffered',
                                  paste0(cv_type_spec, " (", buff_size, "km)"), 
                                  cv_type_spec)) 

others_out <- lapply(output_fits, 
                 function(x) {
                     cv_fit <- x$ctm_fit$others
                     cv_fit$matern_nu <- x$matern.nu
                     cv_fit$cv_type <- x$cv
                     cv_fit$time_fit <- x$time_fit['elapsed'] / 60^2
                     cv_fit$iter <- 1:nrow(cv_fit)
                     return(cv_fit)
                    }
                 ) |>
    (\(.) Reduce(rbind, .))()

cv_out <- merge(cv_out, 
               obs[, c("longitude", "latitude", "space_id", "time_id")],
               by.x = c("space_id", "time_id"),
               by.y = c("space_id", "time_id"),
               all.x = TRUE)



test <- cv_out |>
    filter(cv_type_spec == 'Spatial',
           matern_nu == 0.5)

hist(test$estimate)

test |>
    mutate(model = paste0(cv_type_spec, " - ", matern_nu)) |>
    ggplot(aes(x = estimate, y = obs, color = model)) +
    geom_point(alpha = 0.1) +
    geom_abline(slope = 1, intercept = 0) +
    labs(x = "Prediction", 
         y = "Observation", 
         title = "Prediction vs. Observation",
         subtitle = "Ordinary Cross Validation, Matern Nu = 0.5")

summary(((cv_out$estimate - cv_out$obs)^2))
sd(cv_out$obs)
cv_out |>
    mutate(cover = obs > lower_95 & obs < upper_95) |>
    group_by(matern_nu, cv_type_spec) |>
    summarise(rmse = sqrt(mean((estimate - obs)^2)),
              coverage = mean(cover)) |> 
    mutate(rmse = round(rmse, 2),
           rmse = paste0(rmse, " (", round(coverage, 2), ")")) |>
    select(-coverage) |>
    rename(`Cross Validation Type` = cv_type_spec,
           `Matern Nu Parameter` = matern_nu,
           RMSE = rmse) |>
    pivot_wider(names_from = `Matern Nu Parameter`,
                values_from = RMSE) |>
    arrange(`0.5`) |>
    kable() |>
    kableExtra::add_header_above(c(" " = 1, "Mat\\\\'{e}rn $\\\\nu$ Parameter" = 2), 
                                 align = "c", 
                                 escape = F) |>
    writeLines(paste0(save_dir, "cv_rmse.tex"))


#cv RMSE by monmth
cv_out |>
    mutate(cover = obs > lower_95 & obs < upper_95) |>
    mutate(month = as.numeric(format(date, "%m"))) |>
    group_by(matern_nu, cv_type_spec,  month) |>
    summarise(rmse = sqrt(mean((estimate - obs)^2))) |>
#              coverage = mean(cover),
#              mean_sd = mean(sd),
#              time = unique(time_fit_cv)) |>
    ungroup() |>
    pivot_wider(names_from = month, values_from = rmse) |>
    rename(`Matern Nu` = matern_nu,
           `CV Type` = cv_type_spec) |>
#           `Time (hours)` = time) |>
    rename_at(vars(`1`:`12`), ~ month.abb) |>
    write.csv(paste0(save_dir, "cv_rmse_month.csv"), row.names = F)

cv_out |>
    mutate(cover = obs > lower_95 & obs < upper_95) |>
    mutate(month = as.numeric(format(date, "%m"))) |>
    group_by(matern_nu, cv_type_spec,  month) |>
    summarise(rmse = sqrt(mean((estimate - obs)^2))) |>
#              coverage = mean(cover),
#              mean_sd = mean(sd),
#              time = unique(time_fit_cv)) |>
    ungroup() |>
    pivot_wider(names_from = month, values_from = rmse) |>
    rename(`Matern Nu` = matern_nu,
           `CV Type` = cv_type_spec) |>
#           `Time (hours)` = time) |>
    rename_at(vars(`1`:`12`), ~ month.abb) |>
    kable() |>
    writeLines(paste0(save_dir, "cv_rmse_by_month.tex"))



#cv prediction one day in october
cv_out |>
    group_by(time_id) |>
    summarise(`Max(Observation)` = max(obs),
              `Max(Prediction)` = max(estimate)) |>
    pivot_longer(cols = c(`Max(Observation)`, `Max(Prediction)`),
                 names_to = 'type', 
                 values_to = 'value') |>
    ggplot(aes(x = value)) + 
    geom_histogram() +
    facet_wrap(~ type, nrow = 1) +
    labs(x = "Max Value", 
         y = "Count", 
         title = "Max Observed and Predicted Value by Day")

ggsave(paste0(save_dir, "cv_pred_daily_max_hist.png"), width = 8, height = 4) 

#observed value one day in october
cv_out |>
    filter(time_id == 288) |>
    ggplot(aes(x = x, y = y)) +
    geom_point(aes(color = obs))



#cv prediction one day in october
cv_out |>
    filter(time_id == 288) |>
    ggplot(aes(x = x, y = y)) +
    geom_point(aes(color = estimate))



others_out |>
    ggplot(aes(x = iter, y = theta.alpha)) + 
    geom_line() +
    facet_grid(matern_nu ~ cv_type) +
    labs(x = "Iteration", 
         y = expression(theta[alpha]), 
         title = "Trace Plot of " ~ theta[alpha],
         subtitle = "Facetted By CV Type and Matern Nu Parameter")
ggsave(paste0(save_dir, "theta_alpha_trace.png"), width = 12, height = 6) 

others_out |>
    ggplot(aes(x = iter, y = theta.beta)) + 
    geom_line() +
    facet_grid(matern_nu ~ cv_type) +
    labs(x = "Iteration", 
         y = expression(theta[alpha]), 
         title = "Trace Plot of " ~ theta[beta],
         subtitle = "Facetted By CV Type and Matern Nu Parameter")
ggsave(paste0(save_dir, "theta_beta_trace.png"), width = 12, height = 6) 

others_out |>
    ggplot(aes(x = iter, y = tau.alpha)) + 
    geom_line() +
    facet_grid(matern_nu ~ cv_type, scales = "free") +
    labs(x = "Iteration", 
         y = expression(tau[alpha]), 
         title = "Trace Plot of " ~ tau[alpha],
         subtitle = "Facetted By CV Type and Matern Nu Parameter")
ggsave(paste0(save_dir, "tau_alpha_trace.png"), width = 12, height = 6) 

others_out |>
    ggplot(aes(x = iter, y = tau.beta)) + 
    geom_line() +
    facet_grid(matern_nu ~ cv_type, scales = "free") +
    labs(x = "Iteration", 
         y = expression(tau[alpha]), 
         title = "Trace Plot of " ~ tau[beta],
         subtitle = "Facetted By CV Type and Matern Nu Parameter")
ggsave(paste0(save_dir, "tau_beta_trace.png"), width = 12, height = 6) 

others_out |>
    ggplot(aes(x = iter, y = alpha0)) + 
    geom_line() +
    facet_grid(matern_nu ~ cv_type, scales = "free") +
    labs(x = "Iteration", 
         y = expression(alpha[0]), 
         title = "Trace Plot of " ~ alpha[0],
         subtitle = "Facetted By CV Type and Matern Nu Parameter")
ggsave(paste0(save_dir, "alpha0_trace.png"), width = 12, height = 6) 

others_out |>
    ggplot(aes(x = iter, y = beta0)) + 
    geom_line() +
    facet_grid(matern_nu ~ cv_type, scales = "free") +
    labs(x = "Iteration", 
         y = expression(beta[0]), 
         title = "Trace Plot of " ~ beta[0],
         subtitle = "Facetted By CV Type and Matern Nu Parameter")
ggsave(paste0(save_dir, "beta0_trace.png"), width = 12, height = 6) 

others_out |>
    ggplot(aes(x = iter)) + 
    geom_line(aes(y = alpha0), colour = "blue") +
    geom_line(aes(y = beta0), colour = "red") +
    facet_grid(matern_nu ~ cv_type, scales = "free")

others_out |>
    ggplot(aes(x = iter, y = sigma2)) + 
    geom_line() +
    facet_grid(matern_nu ~ cv_type, scales = "free") +
    labs(x = "Iteration", 
         y = expression(sigma^2), 
         title = "Trace Plot of " ~ sigma^2,
         subtitle = "Facetted By CV Type and Matern Nu Parameter")
ggsave(paste0(save_dir, "sigma2_trace.png"), width = 12, height = 6) 

others_out |>
    ggplot(aes(x = iter, y = rho.alpha)) + 
    geom_line() +
    facet_grid(matern_nu ~ cv_type, scales = "free") +
    labs(x = "Iteration", 
         y = expression(rho[alpha]), 
         title = "Trace Plot of " ~ rho[alpha],
         subtitle = "Facetted By CV Type and Matern Nu Parameter")
ggsave(paste0(save_dir, "rho_alpha_trace.png"), width = 12, height = 6) 

others_out |>
    ggplot(aes(x = iter, y = rho.beta)) + 
    geom_line() +
    facet_grid(matern_nu ~ cv_type, scales = "free") +
    labs(x = "Iteration", 
         y = expression(rho[alpha]), 
         title = "Trace Plot of " ~ rho[beta],
         subtitle = "Facetted By CV Type and Matern Nu Parameter")
ggsave(paste0(save_dir, "rho_beta_trace.png"), width = 12, height = 6) 

others_out |>
    ggplot(aes(x = iter, y = omega.alpha)) + 
    geom_line() +
    facet_grid(matern_nu ~ cv_type, scales = "free") +
    labs(x = "Iteration", 
         y = expression(omega[alpha]), 
         title = "Trace Plot of " ~ omega[alpha],
         subtitle = "Facetted By CV Type and Matern Nu Parameter")
ggsave(paste0(save_dir, "omega_alpha_trace.png"), width = 12, height = 6) 

others_out |>
    ggplot(aes(x = iter, y = omega.beta)) + 
    geom_line() +
    facet_grid(matern_nu ~ cv_type, scales = "free") +
    labs(x = "Iteration", 
         y = expression(omega[beta]), 
         title = "Trace Plot of " ~ omega[beta],
         subtitle = "Facetted By CV Type and Matern Nu Parameter")
ggsave(paste0(save_dir, "omega_beta_trace.png"), width = 12, height = 6) 

others_out |>
    ggplot(aes(x = iter, y = lambda.gamma)) + 
    geom_line() +
    facet_grid(matern_nu ~ cv_type, scales = "free") +
    labs(x = "Iteration", 
         y = expression(lambda[gamma]), 
         title = "Trace Plot of " ~ lambda[gamma],
         subtitle = "Facetted By CV Type and Matern Nu Parameter")
ggsave(paste0(save_dir, "lambda_gamma_trace.png"), width = 12, height = 6) 

others_out |>
    ggplot(aes(x = iter, y = lambda.delta)) + 
    geom_line() +
    facet_grid(matern_nu ~ cv_type, scales = "free") +
    labs(x = "Iteration", 
         y = expression(lambda[delta]), 
         title = "Trace Plot of " ~ lambda[delta],
         subtitle = "Facetted By CV Type and Matern Nu Parameter")
ggsave(paste0(save_dir, "lambda_delta_trace.png"), width = 12, height = 6) 


others_out |>
    distinct(matern_nu, cv_type, time_fit) |>
    ggplot(aes(x = paste(cv_type, matern_nu, sep = " - "), y = time_fit)) +
    geom_bar(stat = 'identity') +
    labs(x = "Model", 
         y = "Time (hours)", 
         title = "Time to Fit Model")
ggsave(paste0(save_dir, "time_fit.png"), width = 8, height = 4)


temp <- output[[1]]
temp$matern.nu
temp$cv

temp$ctm_fit$alpha.time %>%
    pivot_longer(2:ncol(.), 
                 names_to = 'sample', 
                 values_to = 'value') |>
    mutate(sample = as.integer(substr(sample, 7, nchar(sample)))) |>
    ggplot(aes(x = sample, y = value, group = time.id)) + 
    geom_line(alpha = 0.1)

temp$ctm_fit$alpha.time %>%
    pivot_longer(2:ncol(.), 
                 names_to = 'sample', 
                 values_to = 'value') |>
    mutate(sample = as.integer(substr(sample, 7, nchar(sample)))) |>
    group_by(time.id) |>
    summarize(post_mean = mean(value)) |>
    ggplot(aes(x = time.id, y = post_mean)) +
    geom_point()

temp$ctm_fit$alpha.time %>%
    pivot_longer(2:ncol(.), 
                 names_to = 'sample', 
                 values_to = 'value') |>
    mutate(sample = as.integer(substr(sample, 7, nchar(sample)))) |>
    group_by(time.id) |>
    summarize(post_mean = mean(value)) |>
    pull(post_mean) |>
    acf()

temp$ctm_fit$beta.time %>%
    pivot_longer(2:ncol(.), 
                 names_to = 'sample', 
                 values_to = 'value') |>
    mutate(sample = as.integer(substr(sample, 7, nchar(sample)))) |>
    ggplot(aes(x = sample, y = value, group = time.id)) + 
    geom_line(alpha = 0.1)

temp$ctm_fit$beta.time %>%
    pivot_longer(2:ncol(.), 
                 names_to = 'sample', 
                 values_to = 'value') |>
    mutate(sample = as.integer(substr(sample, 7, nchar(sample)))) |>
    group_by(time.id) |>
    summarize(post_mean = mean(value)) |>
    ggplot(aes(x = time.id, y = post_mean)) +
    geom_point()

temp$ctm_fit$beta.time %>%
    pivot_longer(2:ncol(.), 
                 names_to = 'sample', 
                 values_to = 'value') |>
    mutate(sample = as.integer(substr(sample, 7, nchar(sample)))) |>
    group_by(time.id) |>
    summarize(post_mean = mean(value)) |>
    pull(post_mean) |>
    acf()


temp_alpha_time <- temp$ctm_fit$alpha.time %>%
    pivot_longer(2:ncol(.), 
                 names_to = 'sample', 
                 values_to = 'value') |>
    mutate(sample = as.integer(substr(sample, 7, nchar(sample)))) |>
    filter(time.id == 20)

temp_alpha_space <- temp$ctm_fit$alpha.space %>%
    pivot_longer(3:ncol(.), 
                 names_to = 'sample', 
                 values_to = 'value') |>
    mutate(sample = as.integer(substr(sample, 7, nchar(sample)))) |>
    filter(space.id == 20)

temp_alpha0 <- tibble(value = temp$ctm_fit$others$alpha0) |>
    mutate(sample = 1:length(value)) 


plot(temp_alpha_time$value + temp_alpha_space$value + temp_alpha0$value, type = 'l')
    
temp_alpha_0 <- temp$ctm_fit$alpha.0 %>%
    pivot_longer(2:ncol(.), 
                 names_to = 'sample', 
                 values_to = 'value') |>
    mutate(sample = as.integer(substr(sample, 7, nchar(sample))))




########################################################
#####################plots and tables###################
########################################################


# Get world map data and filter for the U.S.
us_map <- map_data("world") |>
  filter(region == "USA") |>
  filter(!(lat > 49 | lat < 24 | long < -125 | long > -66))

ca_map <- map_data("state") %>%
  filter(region == "california")
or_map <- map_data("state") %>%
  filter(region == "oregon")

# Plot
obs |>
  filter(date == '2018-10-08') |>
  ggplot() +
   geom_polygon(data = ca_map, 
                aes(x = long, 
                    y = lat, 
                    group = group), 
                fill = NA, 
                color = "black") +
   geom_polygon(data = or_map, 
                aes(x = long, 
                    y = lat, 
                    group = group), 
                fill = NA, 
                color = "black") +
  geom_point(aes(x = longitude, 
                 y = latitude, 
                 color = pm25)) +
  scale_colour_viridis_c(
    guide = guide_colorbar(
      barwidth = 0.5, 
      barheight = 4,
      title.theme = element_text(size = 8),
      label.theme = element_text(size = 6)
    )) +
  labs(x = "Longitude", 
       y = "Latitude", 
       color = "PM2.5 (ug/m^3)") +
  theme_bw() +
  theme(
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    legend.position = c(1, 0.5), 
    legend.justification = c(1, 0),
    legend.background = element_rect(fill = "transparent", color = "black"),
    legend.key.size = unit(0.8, "cm")
  )

ggsave(paste0(save_dir, "obs_map_20181008.png"), width = 5, height = 8)



# Plot estimates
cv_out |>
  filter(date == '2018-10-08') |>
  filter(matern_nu != 0.5) |>
  ggplot() +
   geom_polygon(data = ca_map, 
                aes(x = long, 
                    y = lat, 
                    group = group), 
                fill = NA, 
                color = "black") +
   geom_polygon(data = or_map, 
                aes(x = long, 
                    y = lat, 
                    group = group), 
                fill = NA, 
                color = "black") +
  geom_point(aes(x = longitude, 
                 y = latitude, 
                 color = estimate)) +
  facet_grid(matern_nu ~ cv_type_spec) +
  scale_colour_viridis_c(
    guide = guide_colorbar(
      barwidth = 0.1, 
      barheight = .8,
      title.theme = element_text(size = 4),
      label.theme = element_text(size = 3)
    )) +
  labs(x = "Longitude", 
       y = "Latitude", 
       color = "Estimated PM2.5 (ug/m^3)") +
  theme_bw() +
  theme(
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    legend.position = c(1, 0.5), 
    legend.justification = c(1, 0),
    legend.background = element_rect(fill = "transparent", color = "black"),
    legend.key.size = unit(0.2, "cm")
  )
ggsave(paste0(save_dir, "est_map_20181008_facet_cv_mat.png"), width = 10, height = 14)


obs |>
    ggplot(aes(x = pm25)) +
    geom_histogram(bins = 100) +
    labs(x = "PM2.5 (ug/m^3)",
         y = "Count") +
    theme(
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
  )
ggsave(paste0(save_dir, "histogram_all.png"), width = 8, height = 5)

obs |>
    mutate(month = month.abb[as.numeric(format(date, "%m"))],
           month = factor(month, levels = month.abb)) |>
    group_by(month) |>
    summarise(
    mean = mean(pm25),
    median = median(pm25),
    min = min(pm25),
    max = max(pm25),
    sd = sd(pm25)
  ) |>
    (\(.) mutate_at(., 2:ncol(.), function(x) sprintf("%.1f", x)))() |>
    rename_all(function(x) paste0(toupper(substr(x, 1, 1)), substr(x, 2, nchar(x)))) |>
    kable() |>
    writeLines(paste0(save_dir, "obs_monthly_stats.tex"))

####spatial buffer justification####
med_theta_alpha <- others_out |>
    filter(cv_type == "ordinary", matern_nu == 0.5) |>
    pull(theta.alpha) |>
    median()

med_theta_beta <- others_out |>
    filter(cv_type == "ordinary", matern_nu == 0.5) |>
    pull(theta.beta) |>
    median()

corr_by_dist <- function(km, theta) {
    exp(-km / theta)
}

corr_by_dist_inv <- function(corr, theta) {
    -theta * log(corr)
}

tibble(km = seq(1, 500, 1)) |>
    mutate(alpha = corr_by_dist(km, med_theta_alpha),
           beta = corr_by_dist(km, med_theta_beta)) |>
    pivot_longer(cols = c(alpha, beta), 
                 names_to = 'theta', 
                 values_to = 'value') |>
    ggplot() +
    geom_line(aes(x = km, y = value, color = theta)) +
    geom_hline(yintercept = 0.8, linetype = 'dashed') +
    geom_hline(yintercept = 0.5, linetype = 'dashed') +
    labs(x = "Distance (km)", 
         y = "Correlation", 
         color = "Theta",
         title = "Correlation by Distance",
         subtitle = "Spatial Buffer Cutoff As Dashed Lines")
ggsave(paste0(save_dir, "corr_by_dist.png"), width = 8, height = 4)

round(mean(corr_by_dist_inv(0.8, med_theta_alpha), corr_by_dist_inv(0.8, med_theta_beta)))
round(mean(corr_by_dist_inv(0.7, med_theta_alpha)))
round(mean(corr_by_dist_inv(0.5, med_theta_alpha), corr_by_dist_inv(0.5, med_theta_beta)))
round(mean(corr_by_dist_inv(0.3, med_theta_alpha)))
corr_by_dist_inv(0.5, med_theta_alpha)
corr_by_dist_inv(0.5, med_theta_beta)


########################################################
#############Estimate plot ######################
########################################################
library(grmbayes)
library(tidyverse)
fit_dat <- readRDS("../../output/results/fits/fit_0.5_ordinary.RDS")
alpha_space <- fit_dat$ctm_fit$alpha.space
alpha_space$mean_alpha_space <- rowMeans(alpha_space[, 3:ncol(alpha_space)])
alpha_space <- alpha_space[, c("space.id", "spacetime.id", "mean_alpha_space")]
beta_space <- fit_dat$ctm_fit$beta.space
beta_space$mean_beta_space <- rowMeans(beta_space[, 3:ncol(beta_space)])
beta_space <- beta_space[, c("space.id", "spacetime.id", "mean_beta_space")]

obs                     
obs_loc <- unique(obs[, c("space_id", "latitude", "longitude", "x", "y")])


alpha_space <- merge(alpha_space,
                     obs_loc,
                     by.x = "space.id",
                     by.y = "space_id",
                     all.x = TRUE)

beta_space <- merge(beta_space,
                     obs_loc,
                     by.x = "space.id",
                     by.y = "space_id",
                     all.x = TRUE)




alpha_space_small <- alpha_space |>
    filter(latitude > 37.25 & latitude < 38.5,
           longitude > -123.5 & longitude < -121.75)

beta_space_small <- beta_space |>
    filter(latitude > 37.25 & latitude < 38.5,
           longitude > -123.5 & longitude < -121.75)

pred_map_small <- alpha_space_small |>
  ggplot() +
  geom_point(aes(x = longitude, 
                 y = latitude, 
                 color = mean_alpha_space)) +
  scale_colour_viridis_c(
    guide = guide_colorbar(
      barwidth = 0.25, 
      barheight = 2,
      title.theme = element_text(size = 4),
      label.theme = element_text(size = 3)
    )) +
  labs(x = "Longitude", 
       y = "Latitude", 
       color = "Mean Alpha Space") +
  theme_bw() +
  theme(
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    legend.position = c(1, 1), 
    legend.justification = c(1, 1),
    legend.background = element_rect(fill = "transparent", color = "black"),
    legend.key.size = unit(0.8, "cm")
  )

ggsave(paste0(save_dir, "alpha_space_est_map_small_20181008.png"), 
       pred_map_small,
       dpi = 600,
       width = 4, 
       height = 3)

pred_map <- alpha_space |>
  ggplot() +
  geom_point(aes(x = longitude, 
                 y = latitude, 
                 color = mean_alpha_space)) +
  scale_colour_viridis_c(
    guide = guide_colorbar(
      barwidth = 0.25, 
      barheight = 2,
      title.theme = element_text(size = 4),
      label.theme = element_text(size = 3)
    )) +
  labs(x = "Longitude", 
       y = "Latitude", 
       color = "Mean Alpha Space") +
  theme_bw() +
  theme(
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    legend.position = c(1, 1), 
    legend.justification = c(1, 1),
    legend.background = element_rect(fill = "transparent", color = "black"),
    legend.key.size = unit(0.8, "cm")
  )

ggsave(paste0(save_dir, "alpha_space_est_map_20181008.png"), 
       pred_map,
       dpi = 600,
       width = 4, 
       height = 3)


pred_map_small <- beta_space_small |>
  ggplot() +
  geom_point(aes(x = longitude, 
                 y = latitude, 
                 color = mean_beta_space)) +
  scale_colour_viridis_c(
    guide = guide_colorbar(
      barwidth = 0.25, 
      barheight = 2,
      title.theme = element_text(size = 4),
      label.theme = element_text(size = 3)
    )) +
  labs(x = "Longitude", 
       y = "Latitude", 
       color = "Mean Beta Space") +
  theme_bw() +
  theme(
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    legend.position = c(1, 1), 
    legend.justification = c(1, 1),
    legend.background = element_rect(fill = "transparent", color = "black"),
    legend.key.size = unit(0.8, "cm")
  )

ggsave(paste0(save_dir, "beta_space_est_map_small_20181008.png"), 
       pred_map_small,
       dpi = 600,
       width = 4, 
       height = 3)

pred_map <- beta_space |>
  ggplot() +
  geom_point(aes(x = x, 
                 y = y, 
                 color = mean_beta_space)) +
  scale_colour_viridis_c(
    guide = guide_colorbar(
      barwidth = 0.25, 
      barheight = 2,
      title.theme = element_text(size = 4),
      label.theme = element_text(size = 3)
    )) +
  labs(x = "Longitude", 
       y = "Latitude", 
       color = "Mean Beta Space") +
  theme_bw() +
  theme(
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    legend.position = c(1, 1), 
    legend.justification = c(1, 1),
    legend.background = element_rect(fill = "transparent", color = "black"),
    legend.key.size = unit(0.8, "cm")
  )

ggsave(paste0(save_dir, "beta_space_est_map_20181008.png"), 
       pred_map,
       dpi = 600,
       width = 4, 
       height = 3)

########################################################
#############prediction plot ######################
########################################################


library(grmbayes)
library(tidyverse)

preds_est <- readRDS("../../output/results/preds/preds.RDS")

preds <- readRDS("../../data/created/preds.rds")
nrow(preds_est)
nrow(preds)
sum(preds_est$space.id != preds$space_id)
sum(preds_est$time.id != preds$time_id)
sum(preds_est$spacetime.id != preds$spacetime_id)
preds$estimate <- preds_est$estimate
preds$sd <- preds_est$sd
preds$alpha_time <- preds_est$alpha_time
preds$beta_time <- preds_est$beta_time
preds$alpha_space <- preds_est$alpha_space
preds$beta_space <- preds_est$beta_space
preds <- merge(preds, 
               locs[, c("longitude", "latitude", "maiac_id")],
               by = "maiac_id", 
               sort = F,
               all.x = TRUE)

preds_est$alpha_space






preds_tmp <- preds |>
    filter(date == '2018-07-08',
           x > -10950000,
           x < -10850000,
           y > 3750000,
           y < 3850000) 

cov_names <- c("elevation", "population",
               "cloud", "v_wind",
               "hpbl", "u_wind",
               "short_rf", "humidity_2m",
               "estimate")

for (cov in cov_names) {

    p_temp <- preds_tmp |>
      ggplot() +
      geom_point(aes(x = x, 
                     y = y,
                     color = get(cov)),
                 shape = 15,
                 size = 1) +
      scale_colour_viridis_c(
        guide = guide_colorbar(
          barwidth = 0.5, 
          barheight = 4,
          title.theme = element_text(size = 8),
          label.theme = element_text(size = 6)
        )) +
      labs(x = "Longitude", 
           y = "Latitude", 
           color = cov) +
      theme_bw() +
      theme(
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        legend.position = c(1, 0.5), 
        legend.justification = c(1, 0),
        legend.background = element_rect(fill = "transparent", color = "black"),
        legend.key.size = unit(0.8, "cm")
      )
    ggsave(paste0(save_dir, "cov_", cov, "_20180708.png"),
           p_temp,
           dpi = 400,
           width = 5, 
           height = 5)

}






preds_one_day <- preds |>
  filter(date == '2018-07-28')

preds_one_day_small <- preds_one_day |>
    filter(latitude > 37.25 & latitude < 38.5,
           longitude > -123.5 & longitude < -121.75)

obs_one_day <- obs |>
  filter(date == '2018-07-28')

obs_one_day_small <- obs_one_day |>
    filter(latitude > 37.25 & latitude < 38.5,
           longitude > -123.5 & longitude < -121.75)

pred_map_20180708 <- preds_one_day |>
  ggplot() +
  geom_point(aes(x = longitude, 
                 y = latitude,
                 color = log(estimate)),
             shape = 15,
             size = 0.005) +
#  geom_point(data = obs |>
#               filter(date == '2018-07-08'),
#             aes(x = longitude, 
#                 y = latitude,
#                 color = log(pm25)),
#             shape = 15,
#             size = 10) +
  scale_colour_viridis_c(
    guide = guide_colorbar(
      barwidth = 0.5, 
      barheight = 4,
      title.theme = element_text(size = 8),
      label.theme = element_text(size = 6)
    )) +
  labs(x = "Longitude", 
       y = "Latitude", 
       color = "PM2.5 (ug/m^3)") +
  theme_bw() +
  theme(
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    legend.position = c(1, 0.5), 
    legend.justification = c(1, 0),
    legend.background = element_rect(fill = "transparent", color = "black"),
    legend.key.size = unit(0.8, "cm")
  )

ggsave(paste0(save_dir, "pred_map_20180728.png"), 
       pred_map_20180708,
       dpi = 600,
       width = 10, 
       height = 16)


pred_map_small_20180708 <- preds_one_day_small |>
  ggplot() +
  geom_point(aes(x = longitude, 
                 y = latitude,
                 color = log(estimate)),
             shape = 15,
             size = 0.4) +
  geom_point(data = obs_one_day_small,
             aes(x = longitude, 
                 y = latitude),
             shape = 21,
             size = 3,
             alpha = 0.3) +
  scale_colour_viridis_c(
    guide = guide_colorbar(
      barwidth = 0.4, 
      barheight = 4,
      title.theme = element_text(size = 8),
      label.theme = element_text(size = 6)
    )) +
  labs(x = "Longitude", 
       y = "Latitude", 
       color = "PM2.5 (ug/m^3)") +
  theme_bw() +
  theme(
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    legend.position = c(0, 0), 
    legend.justification = c(0, 0),
    legend.background = element_rect(fill = "transparent", color = "black"),
    legend.key.size = unit(3, "cm")
  )

ggsave(paste0(save_dir, "pred_map_small_20180728.png"), 
       pred_map_small_20180708,
       dpi = 600,
       width = 4, 
       height = 4)

preds_one_day_small[is.na(preds_one_day_small$alpha_space), c("longitude", "latitude")]
obs_one_day_small[, c("longitude", "latitude")]
nrow(obs_one_day_small)
pred_map_small_20180708_alpha_space <- preds_one_day_small |>
  ggplot() +
  geom_point(aes(x = longitude, 
                 y = latitude,
                 color = alpha_space),
             shape = 15,
             size = 0.4) +
  geom_point(data = obs_one_day_small,
             aes(x = longitude, 
                 y = latitude),
             shape = 21,
             size = 3,
             alpha = 0.3) +
  scale_colour_viridis_c(
    guide = guide_colorbar(
      barwidth = 0.4, 
      barheight = 4,
      title.theme = element_text(size = 8),
      label.theme = element_text(size = 6)
    )) +
  labs(x = "Longitude", 
       y = "Latitude", 
       color = "Spatial Intercept") +
  theme_bw() +
  theme(
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    legend.position = c(0, 0), 
    legend.justification = c(0, 0),
    legend.background = element_rect(fill = "transparent", color = "black"),
    legend.key.size = unit(3, "cm")
  )

ggsave(paste0(save_dir, "pred_map_small_20180728_alpha_space.png"), 
       pred_map_small_20180708_alpha_space,
       dpi = 600,
       width = 4, 
       height = 4)
nrow(preds_one_day_small[is.na(preds_one_day_small$beta_space), c("longitude", "latitude")])
nrow(obs_one_day_small[, c("longitude", "latitude")])
pred_map_small_20180708_beta_space <- preds_one_day_small |>
  ggplot() +
  geom_point(aes(x = longitude, 
                 y = latitude,
                 color = beta_space),
             shape = 15,
             size = 0.4) +
  geom_point(data = obs_one_day_small,
             aes(x = longitude, 
                 y = latitude),
             shape = 21,
             size = 3,
             alpha = 0.3) +
  scale_colour_viridis_c(
    guide = guide_colorbar(
      barwidth = 0.4, 
      barheight = 4,
      title.theme = element_text(size = 8),
      label.theme = element_text(size = 6)
    )) +
  labs(x = "Longitude", 
       y = "Latitude", 
       color = "Spatial Slope") +
  theme_bw() +
  theme(
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    legend.position = c(0, 0), 
    legend.justification = c(0, 0),
    legend.background = element_rect(fill = "transparent", color = "black"),
    legend.key.size = unit(3, "cm")
  )

ggsave(paste0(save_dir, "pred_map_small_20180728_beta_space.png"), 
       pred_map_small_20180708_beta_space,
       dpi = 600,
       width = 4, 
       height = 4)


table(obs$date)
obs

preds |>
    filter(date %in% c('2018-07-08', '2018-07-28')) |>
    filter(space_id == 5000) |>
    select(estimate)

           


ctm |>
  filter(date == '2018-10-08') |>
  ggplot() +
  geom_point(aes(x = grid_lon.y, 
                 y = grid_lat.y,
                 color = pm25_tot_ncar),
             shape = 15,
             size = 0.8) +
  scale_colour_viridis_c(
    guide = guide_colorbar(
      barwidth = 0.5, 
      barheight = 4,
      title.theme = element_text(size = 8),
      label.theme = element_text(size = 6)
    )) +
  labs(x = "Longitude", 
       y = "Latitude", 
       color = "PM2.5 (ug/m^3)") +
  theme_bw() +
  theme(
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    legend.position = c(1, 0.5), 
    legend.justification = c(1, 0),
    legend.background = element_rect(fill = "transparent", color = "black"),
    legend.key.size = unit(0.8, "cm")
  )

ggsave(paste0(save_dir, "ctm_map_20181008.png"), width = 8, height = 5)


sum(is.na(ctm_pred$estimate))
#posterior means by season map
preds |>
  mutate(month = as.numeric(format(date, "%m")),
         month = month.abb[month],
         month = factor(month, levels = month.abb),
         season = case_when(month %in% month.abb[1:3] ~ "Jan - Mar",
                            month %in% month.abb[4:6] ~ "Apr - Jun",
                            month %in% month.abb[7:9] ~ "Jul - Sep",
                            month %in% month.abb[10:12] ~ "Oct - Dec")) |>
  group_by(season, space_id, spacetime_id, latitude, longitude) |>
  summarize(estimate = mean(estimate, na.rm = T)) |>
  ggplot() +
  geom_point(aes(x = longitude, 
                 y = latitude, 
                 color = estimate),
             shape = 15,
             size = 0.8) +
  scale_colour_viridis_c(
    guide = guide_colorbar(
      barwidth = 0.25, 
      barheight = 2,
      title.theme = element_text(size = 4),
      label.theme = element_text(size = 3)
    )) +
  labs(x = "Longitude", 
       y = "Latitude", 
       color = "PM2.5 (ug/m^3)") +
  theme_bw() +
  facet_wrap(~ season, ncol = 2) +
  theme(
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    legend.position = c(1, 0.5), 
    legend.justification = c(1, 0),
    legend.background = element_rect(fill = "transparent", color = "black"),
    legend.key.size = unit(0.8, "cm")
  )

ggsave(paste0(save_dir, "pred_map_season.png"), width = 8, height = 5)







########################################################
############# within-sample preds ######################
########################################################

ctm_pred_within_sample <- readRDS("../../output/results/within_sample_preds/preds_0.5_ordinary_within_sample.RDS")
obs
ctm_pred_ws <- merge(ctm_pred_within_sample, 
                obs[, c("pm25", "longitude", "latitude", "space_id", "time_id")],
                by.x = c("space.id", "time.id"),
                by.y = c("space_id", "time_id"),
                all.x = TRUE) 
tail(ctm_pred_ws)

ctm_pred_ws |>
    ggplot(aes(x = estimate, y = pm25)) +
    geom_point(alpha = 0.1) +
    geom_abline(slope = 1, intercept = 0) +
    labs(x = "Prediction", 
         y = "Observation", 
         title = "Prediction vs. Observation",
         subtitle = "Ordinary Cross Validation, Matern Nu = 0.5")

#rmse
ctm_pred_ws |>
    mutate(mse = (estimate - pm25)^2) |>
    summarize(mse = mean(mse, na.rm = T)) |>
    pull(mse) |>
    sqrt()




X <- as.matrix(obs[, c("elevation", "population",
                       "cloud", "v_wind", "hpbl",
                       "u_wind", "short_rf", "humidity_2m",
                       "time_id", "space_id")])
X <- as.matrix(obs[, c("elevation", "population",
                       "cloud", "v_wind", "hpbl",
                       "u_wind", "short_rf", "humidity_2m")])
y <- obs$pm25



# Fit elastic net model
# alpha is the mixing parameter (0 <= alpha <= 1)
# lambda is the regularization parameter
#library(glmnet)
set.seed(123)  
glm_fit_cv <- glmnet::cv.glmnet(X, y, alpha = 0.5, type.measure = "mse")
glm_fit <- glmnet::glmnet(X, y, alpha = 0.5, type.measure = "mse")
best_lambda <- glm_fit_cv$lambda.min
glmnet_preds <- glmnet:::predict.glmnet(object = glm_fit, 
                                           newx = X, 
                                           s = best_lambda)
tail(glmnet_preds)
#rmse
sqrt(mean((glmnet_preds - y)^2))

# Fit Random Forest model
library(randomForest)
set.seed(123)
rf_fit <- randomForest::randomForest(x = X, y = y, ntree = 1000, do.trace = 1)
rf_preds <- predict(rf_fit, X)
#rmse
sqrt(mean((rf_preds - y)^2))


#############################################
############# cv preds ######################
#############################################

ctm_pred_cv_all <- readRDS("../../output/results/fits/fit_0.5_ordinary.RDS")

obs_cv <- obs[!is.na(ctm_pred_cv$ctm_fit_cv$estimate), ]

ctm_pred_cv <- ctm_pred_cv_all$ctm_fit_cv[!is.na(ctm_pred_cv$ctm_fit_cv$estimate), ]

sqrt(mean((ctm_pred_cv$estimate - ctm_pred_cv$obs)^2))



X_cv <- as.matrix(obs_cv[, c("elevation", "population",
                       "cloud", "v_wind", "hpbl",
                       "u_wind", "short_rf", "humidity_2m")])
X_cv <- as.matrix(obs_cv[, c("elevation", "population",
                       "cloud", "v_wind", "hpbl",
                       "u_wind", "short_rf", "humidity_2m",
                       "time_id", "space_id")])
y_cv <- obs_cv$pm25


# Fit elastic net model cv
set.seed(123)  # for reproducibility
glm_folds <- caret::createFolds(y_cv, k = 10, list = TRUE)  # Replace 5 with desired number of folds
glm_cv_preds <- rep(NA, length(y_cv))

for(i in 1:length(glm_folds)) {
  # Split the data
  train_indices <- unlist(glm_folds[-i])
  test_indices <- unlist(glm_folds[i])
  X_train <- X_cv[train_indices, ]
  y_train <- y_cv[train_indices]
  X_test <- X_cv[test_indices, ]

  # Fit the model
  fit <- glmnet::cv.glmnet(X_train, y_train)

  # Predict on the test set
  predictions <- glmnet:::predict.cv.glmnet(fit, newx = X_test, s = "lambda.min")

  # Store the predictions
  glm_cv_preds[test_indices] <- predictions
}
sqrt(mean((glm_cv_preds - ctm_pred_cv$obs)^2))


glm_fit_cv <- glmnet::cv.glmnet(X, y, alpha = 0.5, type.measure = "mse")
best_lambda <- glm_fit_cv$lambda.min
glmnet_preds_cv <- glmnet:::predict.cv.glmnet(object = glm_fit_cv, 
                                              newx = X, 
                                              s = best_lambda)
#rmse
sqrt(mean((glmnet_preds_cv - y)^2))




# Fit Random Forest model cv
data_combined <- data.frame(response = y_cv, X_cv)
control <- caret::trainControl(method = "cv", number = 10)
rf_model <- caret::train(response ~ ., data = data_combined, method = "rf", trControl = control)
rf_cv_preds <- rf_model$pred$pred







