library(tidyverse)
library(knitr)
library(kableExtra)
library(maps)
theme_set(theme_bw())

kable <- function(x, digits = 2, ...) {
    x |>
    knitr::kable("latex", 
                 booktabs = T, 
                 align = "c", 
                 digits = digits,
                 linesep = "\\\\[-3.0ex]",
                 ...)
}


#read in RData file
load("../../data/howard_uploaded/Grid_PM25.RData")
locs <- grid.info
names(locs) <- tolower(names(locs))


obs <- readRDS("../../data/created/obs.rds")
obs <- merge(obs, 
             locs[, c("grid_lon", "grid_lat", "grid_cell")],
             by.x = "grid_cell", 
             by.y = "grid_cell", 
             all.x = TRUE)
#get distance matrix
temp <- obs[, c("x", "y")] |> distinct() |>dist()
summary(temp)

obs |>
    select(x, y) |>
    distinct() |>
    ggplot(aes(x = x, y = y)) +
    geom_point() +
    coord_fixed()




output <- lapply(list.files("../../output/results/", full.names = T), readRDS)



sd(obs$pm25)
summary(obs$pm25)
summary(cv_out$estimate)

cv_out <- lapply(output, 
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

cv_out <- merge(cv_out, 
               obs[, c("grid_lon", "grid_lat", "space_id", "time_id")],
               by.x = c("space_id", "time_id"),
               by.y = c("space_id", "time_id"),
               all.x = TRUE)

test <- cv_out |>
    filter(cv_type_spec == 'Ordinary',
           matern_nu == 1.5)



test |>
    ggplot(aes(x = estimate, y = obs)) +
    geom_point(alpha = 0.1) +
    geom_abline(slope = 1, intercept = 0) +
    labs(x = "Prediction", 
         y = "Observation", 
         title = "Prediction vs. Observation",
         subtitle = "Ordinary Cross Validation, Matern Nu = 0.5")

hist(((test$estimate - test$obs)^2))
summary(test$estimate)
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

sd(obs$pm_aqs)
#cv RMSE by monmth
cv_out |>
    mutate(cover = obs > lower_95 & obs < upper_95) |>
    mutate(month = as.numeric(format(date, "%m"))) |>
    group_by(matern_nu, cv_type_spec,  month) |>
    summarise(rmse = sqrt(mean((estimate - obs)^2)),
              coverage = mean(cover),
              mean_sd = mean(sd),
              time = unique(time_fit_cv)) |>
    filter(cv_type_spec == "Ordinary")
    pivot_wider(names_from = month, values_from = rmse) |>
    rename(NNGP = nngp,
           `Theta Discretization` = discrete_theta,
           `Time (hours)` = time) |>
    rename_at(vars(`1`:`12`), ~ month.abb) |>
    write.csv(paste0(save_dir, "cv_rmse_month.csv"), row.names = F)
    write_table("cv_rmse_month.md")
#cv RMSE by monmth
cv_out |>
    mutate(month = as.numeric(format(date, "%m"))) |>
    group_by(nngp, discrete_theta, month) |>
    summarise(rmse = sqrt(mean((estimate - obs)^2)), 
              time = unique(time_fit_cv)) |>
    pivot_wider(names_from = month, values_from = rmse) |>
    rename(NNGP = nngp,
           `Theta Discretization` = discrete_theta,
           `Time (hours)` = time) |>
    rename_at(vars(`1`:`12`), ~ month.abb) |>
    write.csv(paste0(save_dir, "cv_rmse_month.csv"), row.names = F)
    write_table("cv_rmse_month.md")


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

#cv prediction one day in october
cv_out |>
    filter(time_id == 288) |>
    ggplot(aes(x = x, y = y)) +
    geom_point(aes(color = obs))



#observed value one day in october
cv_out |>
    filter(time_id == 288) |>
    ggplot(aes(x = x, y = y)) +
    geom_point(aes(color = estimate))



others_out <- lapply(output, 
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

others_out |>
    ggplot(aes(x = iter, y = theta.alpha)) + 
    geom_line() +
    facet_grid(matern_nu ~ cv_type, scales = "free") +
    labs(x = "Iteration", 
         y = expression(theta[alpha]), 
         title = "Trace Plot of " ~ theta[alpha],
         subtitle = "Facetted By CV Type and Matern Nu Parameter")
ggsave(paste0(save_dir, "theta_alpha_trace.png"), width = 12, height = 6) 

others_out |>
    ggplot(aes(x = iter, y = theta.beta)) + 
    geom_line() +
    facet_grid(matern_nu ~ cv_type, scales = "free") +
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
    distinct(nngp, discrete_theta, time_fit) |>
    ggplot(aes(x = paste(nngp, discrete_theta, sep = " - "), y = time_fit)) +
    geom_bar(stat = 'identity') +
    labs(x = "Model", 
         y = "Time (hours)", 
         title = "Time to Fit Model")
ggsave(paste0(save_dir, "time_fit.png"), width = 8, height = 4)

others_out |>
    filter(nngp == 'nngp', discrete_theta == 'gibbs') |>
    ggplot(aes(x = iter, y =  theta.beta)) +
    geom_line()

length(unique(obs$space_id))


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
    legend.position = c(1, 0), 
    legend.justification = c(1, 0),
    legend.background = element_rect(fill = "transparent", color = "black"),
    legend.key.size = unit(0.8, "cm")
  )

ggsave(paste0(save_dir, "obs_map_20181008.png"), width = 8, height = 5)


# Plot cmaq (no ca)
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
                 color = aod.final)) +
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
    legend.position = c(1, 0), 
    legend.justification = c(1, 0),
    legend.background = element_rect(fill = "transparent", color = "black"),
    legend.key.size = unit(0.8, "cm")
  )

ggsave(paste0(save_dir, "obs_map_20181008_no_ca.png"), width = 8, height = 5)


# Plot estimates
cv_out |>
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
                 color = estimate)) +
  facet_grid(matern_nu ~ cv_type_spec) +
  scale_colour_viridis_c(
    guide = guide_colorbar(
      barwidth = 0.5, 
      barheight = 4,
      title.theme = element_text(size = 8),
      label.theme = element_text(size = 6)
    )) +
  labs(x = "Longitude", 
       y = "Latitude", 
       color = "Estimated PM2.5 (ug/m^3)") +
  theme_bw() +
  theme(
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    legend.position = c(1, 0), 
    legend.justification = c(1, 0),
    legend.background = element_rect(fill = "transparent", color = "black"),
    legend.key.size = unit(0.8, "cm")
  )
ggsave(paste0(save_dir, "est_map_20181008_facet_cv_mat.png"), width = 8, height = 5)


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
    filter(nngp == 'gp', discrete_theta == 'none') |>
    pull(theta.alpha) |>
    median()

med_theta_beta <- others_out |>
    filter(nngp == 'gp', discrete_theta == 'none') |>
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
round(mean(corr_by_dist_inv(0.5, med_theta_alpha), corr_by_dist_inv(0.5, med_theta_beta)))
corr_by_dist_inv(0.5, med_theta_alpha)
corr_by_dist_inv(0.5, med_theta_beta)



########################################################
#############prediction plot ######################
########################################################


load("../../data/Grid_PM25.RData")
library(grmbayes)
library(tidyverse)

obs <- OBS |> 
    rename_all(tolower)

grid.info <- grid.info |> 
    rename_all(tolower)

ctm <- CTM |> 
    rename_all(tolower) |> 
    left_join(grid.info,
              by = "grid_cell")
ctm$time_id <- as.integer(factor(ctm$date))
ctm$spacetime_id <- rep(1, nrow(ctm))
grid.info
table(ctm$date)


ctm_fit <- readRDS("../../hpc/output/data/full_grid/full_grid_0.5_ord.RDS")
ctm_fit <- ctm_fit$ctm_fit

#ctm_pred <- grm_pred(grm.fit = ctm_fit,
#                     X.pred = ctm$pm25_tot_ncar,
#                     coords.Y = obs[, c("x", "y")],
#                     space.id.Y = obs$aqs_site_id,
#                     coords.pred = ctm[, c("x", "y")],
#                     space.id = ctm$grid_cell,
#                     time.id = ctm$time_id,
#                     spacetime.id = ctm$spacetime_id,
#                     include.additive.annual.resid = T,
#                     include.multiplicative.annual.resid = T,
#                     n.iter = 1000,
#                     verbose = T)
ctm_pred <- readRDS("../../output/data/grid_data_full_grid_pred_0.5.RDS")

ctm_pred$date <- ctm$date
ctm_pred$lat <- ctm$grid_lat.y
ctm_pred$lon <- ctm$grid_lon.y
    


ctm_pred |>
  filter(date == '2018-10-08') |>
  ggplot() +
  geom_point(aes(x = lon, 
                 y = lat, 
                 color = estimate),
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
    legend.position = c(1, 0), 
    legend.justification = c(1, 0),
    legend.background = element_rect(fill = "transparent", color = "black"),
    legend.key.size = unit(0.8, "cm")
  )

ggsave(paste0(save_dir, "pred_map_20181008.png"), width = 8, height = 5)

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
    legend.position = c(1, 0), 
    legend.justification = c(1, 0),
    legend.background = element_rect(fill = "transparent", color = "black"),
    legend.key.size = unit(0.8, "cm")
  )

ggsave(paste0(save_dir, "ctm_map_20181008.png"), width = 8, height = 5)


sum(is.na(ctm_pred$estimate))
#posterior means by season map
ctm_pred |>
  mutate(month = as.numeric(format(date, "%m")),
         month = month.abb[month],
         month = factor(month, levels = month.abb),
         season = case_when(month %in% month.abb[1:3] ~ "Jan - Mar",
                            month %in% month.abb[4:6] ~ "Apr - Jun",
                            month %in% month.abb[7:9] ~ "Jul - Sep",
                            month %in% month.abb[10:12] ~ "Oct - Dec")) |>
  group_by(season, space.id, spacetime.id, lat, lon) |>
  summarize(estimate = mean(estimate, na.rm = T)) |>
  ggplot() +
  geom_point(aes(x = lon, 
                 y = lat, 
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
    legend.position = c(1, 0), 
    legend.justification = c(1, 0),
    legend.background = element_rect(fill = "transparent", color = "black"),
    legend.key.size = unit(0.8, "cm")
  )

ggsave(paste0(save_dir, "pred_map_season.png"), width = 8, height = 5)
