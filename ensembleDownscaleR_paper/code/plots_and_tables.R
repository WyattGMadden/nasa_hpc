library(tidyverse)
library(patchwork)
theme_classic2 <- function() {
  theme_classic() +
  theme(panel.border = element_rect(colour = "black", fill = NA, linewidth = 1))
}
theme_set(theme_classic2())


monitor_pm25_with_cmaq <- readRDS("../data/monitor_pm25_with_cmaq.rds")
monitor_pm25_with_aod <- readRDS("../data/monitor_pm25_with_aod.rds")

cmaq_fit <- readRDS("../output/fit_pred_objects/cmaq_fit.rds")
cmaq_fit_cv <- readRDS("../output/fit_pred_objects/cmaq_fit_cv.rds")
aod_fit <- readRDS("../output/fit_pred_objects/aod_fit.rds")
aod_fit_cv <- readRDS("../output/fit_pred_objects/aod_fit_cv.rds")
cmaq_for_predictions <- readRDS("../data/cmaq_for_predictions.rds")
cmaq_pred <- readRDS("../output/fit_pred_objects/cmaq_pred.rds")
aod_for_predictions <- readRDS("../data/aod_for_predictions.rds")
aod_pred <- readRDS("../output/fit_pred_objects/aod_pred.rds")
ensemble_fit <- readRDS("../output/fit_pred_objects/ensemble_fit.rds")
weight_preds <- readRDS("../output/fit_pred_objects/weight_preds.rds")
results <- readRDS("../output/fit_pred_objects/results.rds")
ensemble_preds_at_observations <- readRDS("../output/fit_pred_objects/ensemble_preds_at_observations.rds")
runtime <- readRDS("../output/fit_pred_objects/total_hours.rds")

# comparison table results
aod_fit_cv_spat <- readRDS("../output/additional_cv_fit_objects/aod_fit_cv_spat.rds")
cmaq_fit_cv_spat <- readRDS("../output/additional_cv_fit_objects/cmaq_fit_cv_spat.rds")
ensemble_preds_at_observations_spat <- readRDS("../output/additional_cv_fit_objects/ensemble_preds_at_observations_spat.rds")
aod_fit_cv_spatclust <- readRDS("../output/additional_cv_fit_objects/aod_fit_cv_spatclust.rds")
cmaq_fit_cv_spatclust <- readRDS("../output/additional_cv_fit_objects/cmaq_fit_cv_spatclust.rds")
ensemble_preds_at_observations_spatclust <- readRDS("../output/additional_cv_fit_objects/ensemble_preds_at_observations_spatclust.rds")
aod_fit_cv_spatbuff3 <- readRDS("../output/additional_cv_fit_objects/aod_fit_cv_spatbuff3.rds")
cmaq_fit_cv_spatbuff3 <- readRDS("../output/additional_cv_fit_objects/cmaq_fit_cv_spatbuff3.rds")
ensemble_preds_at_observations_spatbuff3 <- readRDS("../output/additional_cv_fit_objects/ensemble_preds_at_observations_spatbuff3.rds")
aod_fit_cv_spatbuff7 <- readRDS("../output/additional_cv_fit_objects/aod_fit_cv_spatbuff7.rds")
cmaq_fit_cv_spatbuff7 <- readRDS("../output/additional_cv_fit_objects/cmaq_fit_cv_spatbuff7.rds")
ensemble_preds_at_observations_spatbuff7 <- readRDS("../output/additional_cv_fit_objects/ensemble_preds_at_observations_spatbuff7.rds")



full_cmaq_preds <- cmaq_for_predictions |>
    left_join(cmaq_pred, by = c("time_id" = "time.id", 
                                "space_id" = "space.id", 
                                "spacetime_id" = "spacetime.id"))
full_aod_preds <- aod_for_predictions |>
    left_join(aod_pred, by = c("time_id" = "time.id", 
                                "space_id" = "space.id", 
                                "spacetime_id" = "spacetime.id"))


# Data - study area calculations 
range(cmaq_for_predictions$longitude)
range(cmaq_for_predictions$latitude)
range(aod_for_predictions$longitude)
range(aod_for_predictions$latitude)

# Data - temporal range
range(monitor_pm25_with_cmaq$date)
range(monitor_pm25_with_cmaq$date)

# Number of grid cells
length(unique(cmaq_for_predictions$space_id))
length(unique(aod_for_predictions$space_id))


# Data - number of monitors
length(unique(monitor_pm25_with_cmaq$space_id))

mon_cmaq_miss <- monitor_pm25_with_cmaq |>
    count(space_id, date) |>
    count(date) |>
    pull(n) |>
    summary()
(60 - mon_cmaq_miss) / 60

mon_aod_miss <- monitor_pm25_with_aod |>
    count(space_id, date) |>
    count(date) |>
    pull(n) |>
    summary()
(60 - mon_aod_miss) / 60

# Data - Grid cells missingness
cmaq_for_predictions |>
    group_by(time_id) |>
    summarize(n = n()) |>
    pull(n) |>
    unique()

aod_miss <- aod_for_predictions |>
    group_by(time_id) |>
    summarize(n = n()) |>
    pull(n) |>
    summary() 

(length(unique(cmaq_for_predictions$space_id)) - aod_miss) / length(unique(cmaq_for_predictions$space_id))


#######################
### Study Area Plot ###
#######################
# Data - study area plot
ca_map <- map_data("state") %>%
  filter(region == "california")

# Define the coordinates for the dotted square
minlon <- min(cmaq_for_predictions$longitude)
maxlon <- max(cmaq_for_predictions$longitude)
minlat <- min(cmaq_for_predictions$latitude)
maxlat <- max(cmaq_for_predictions$latitude)
square <- data.frame(
    long = c(minlon, maxlon, maxlon, minlon, minlon),
    lat = c(minlat, minlat, maxlat, maxlat, minlat)
)


data_plt <- ggplot() +
  geom_polygon(data = ca_map, 
               aes(x = long, y = lat, group = group),
               fill = NA, color = "black") +
  geom_point(data = full_cmaq_preds |> distinct(longitude, latitude),
             aes(x = longitude, y = latitude, color = "Grid Cell"),
             shape = 15,
             size = 0.01) +
  geom_point(data = monitor_pm25_with_cmaq |> distinct(longitude, latitude),
             aes(x = longitude, y = latitude, color = "Monitor Location"),
             shape = 2,
             size = 1) +
  labs(x = "Longitude", 
       y = "Latitude",
       color = NULL) +
  scale_color_manual(
    values = c("Grid Cell" = "#FFEB99", 
               "Monitor Location" = "#7B3294"),
    guide  = guide_legend(
      override.aes = list(
        shape = c(15, 2),
        size  = c(2, 3)
      )
    )
  ) +
  scale_linetype_manual(guide = "none", values = "dotted") +
  theme(legend.position            = "inside",
        legend.position.inside     = c(0.915, 0.94),
        legend.justification.inside = c(0.9, 0.9),
        legend.background          = element_rect(fill = "white", colour = "black"))

data_plt
scale_factor <- 0.6
ggsave(
    "../output/figures/studyarea.png", 
    data_plt, 
    width = 6 * scale_factor,
    height = 6 * scale_factor,
    dpi = 600
)


# compute grid resolution
#data_pltb <- ggplot() +
#  geom_polygon(data = ca_map,
#    aes(x = long, y = lat, group = group),
#    fill = NA, color = "black"
#  ) +
#  geom_point(data = full_cmaq_preds |> distinct(longitude, latitude),
#    aes(x = longitude, y = latitude),
#    shape = 15,
#    size = 0.01,
#  ) +
#  labs(x = "Longitude", y = "Latitude", color = NULL) +
#  coord_quickmap(xlim = c(minlon, maxlon), ylim = c(minlat, maxlat),
#    expand = FALSE
#  ) +
#  theme(
#    legend.position = "inside",
#    legend.position.inside = c(0.915, 0.94),
#    legend.justification.inside = c(0.9, 0.9),
#    legend.background = element_rect(fill = "white", colour = "black")
#  )
#
#
#data_plt_full <- (data_plt + data_pltb) + 
#    plot_layout(ncol = 2, byrow = T) +
#    plot_annotation(tag_levels = "A")
#                                 
#scale_factor <- 0.6
#ggsave(
#    "../output/figures/studyarea.png", 
#    data_plt_full, 
#    width = 6 * scale_factor,
#    height = 6 * scale_factor,
#    dpi = 600
#)


####################
### Stage 2 Plot ###
####################
date_use <- "2018-07-15"
latbuffer <- 0
lonbuffer <- 0
cmaqpredplt <- full_cmaq_preds |>
    filter(date == date_use,
           !is.na(estimate)) |>
    ggplot(aes(x = longitude, y = latitude, colour = estimate)) +
    geom_polygon(data = ca_map, 
                 aes(x = long, y = lat, group = group),
                 fill = NA, color = "black") +
    geom_tile(linewidth = .5) +
    scale_color_viridis_c() +
    coord_cartesian(xlim = c(minlon - lonbuffer, maxlon + lonbuffer), 
                    ylim = c(minlat - latbuffer, maxlat + latbuffer)) +
    labs(title = "CTM-Based PM2.5 Predictions",
         x = "Longitude",
         y = "Latitude",
         colour = "PM2.5 Prediction (µg/m\u00B3)") +
    theme(legend.position = "bottom",
          legend.direction = "horizontal")

cmaqorigplt <- full_cmaq_preds |>
    filter(date == date_use) |>
    ggplot(aes(x = longitude, y = latitude, colour = cmaq)) +
    geom_polygon(data = ca_map, 
                 aes(x = long, y = lat, group = group),
                 fill = NA, color = "black") +
    geom_tile(linewidth = .5) +
    scale_color_viridis_c() +
    coord_cartesian(xlim = c(minlon - lonbuffer, maxlon + lonbuffer), 
                    ylim = c(minlat - latbuffer, maxlat + latbuffer)) +
    labs(title = "CTM PM2.5 Simulations",
         x = "Longitude",
         y = "Latitude",
         colour = "CTM (µg/m\u00B3)") +
    theme(legend.position = "bottom",
          legend.direction = "horizontal")

aodpredplt <- full_aod_preds |>
    filter(date == date_use,
           !is.na(estimate)) |>
    ggplot(aes(x = longitude, y = latitude, colour = estimate)) +
    geom_polygon(data = ca_map, 
                 aes(x = long, y = lat, group = group),
                 fill = NA, color = "black") +
    geom_tile(linewidth = .5) +
    scale_color_viridis_c() +
    coord_cartesian(xlim = c(minlon - lonbuffer, maxlon + lonbuffer), 
                    ylim = c(minlat - latbuffer, maxlat + latbuffer)) +
    labs(title = "AOD-Based PM2.5 Predictions",
         x = "Longitude",
         y = "Latitude",
         colour = "PM2.5 Prediction (µg/m\u00B3)") +
    theme(legend.position = "bottom",
          legend.direction = "horizontal")

aodorigplt <- full_aod_preds |>
    filter(date == date_use) |>
    ggplot(aes(x = longitude, y = latitude, colour = aod)) +
    geom_polygon(data = ca_map, 
                 aes(x = long, y = lat, group = group),
                 fill = NA, color = "black") +
    geom_tile(linewidth = .5) +
    scale_color_viridis_c() +
    coord_cartesian(xlim = c(minlon - lonbuffer, maxlon + lonbuffer), 
                    ylim = c(minlat - latbuffer, maxlat + latbuffer)) +
    labs(title = "Observed AOD",
         x = "Longitude",
         y = "Latitude",
         colour = "AOD") +
    theme(legend.position = "bottom",
          legend.direction = "horizontal")

#join 4 plots
stage2plt <- (cmaqpredplt + cmaqorigplt + aodpredplt + aodorigplt) + 
    plot_layout(ncol = 2, byrow = T) +
    plot_annotation(tag_levels = "A")

scale_factor <- 0.6
ggsave(
    "../output/figures/stage2.png", 
    stage2plt, 
    width = 11 * scale_factor,
    height = 12 * scale_factor,
    dpi = 600
)

####################
### Stage 4 Plot ###
####################

size_use <- 2.5
stage4aplt <- monitor_pm25_with_cmaq |>
    filter(date == date_use) |>
    ggplot(aes(x = longitude, y = latitude, colour = pm25)) +
    geom_polygon(data = ca_map, 
                 aes(x = long, y = lat, group = group),
                 fill = NA, color = "black") +
    geom_point(size = size_use) +
    scale_color_viridis_c() +
    coord_cartesian(xlim = c(minlon - lonbuffer, maxlon + lonbuffer), 
                    ylim = c(minlat - latbuffer, maxlat + latbuffer)) +
    labs(x = "Longitude",
         y = "Latitude",
         colour = "PM2.5 (µg/m\u00B3)",
         title = "PM2.5 Monitor Observation") +
    theme(legend.position = "bottom",
          legend.direction = "horizontal")

stage4bplt <- monitor_pm25_with_cmaq |>
    left_join(cmaq_fit_cv, 
              by = c("time_id" = "time.id", 
                     "space_id" = "space.id", 
                     "spacetime_id" = "spacetime.id")) |>
    filter(date == date_use) |>
    ggplot(aes(x = longitude, y = latitude, colour = estimate)) +
    geom_polygon(data = ca_map, 
                 aes(x = long, y = lat, group = group),
                 fill = NA, color = "black") +
    geom_point(size = size_use) +
    scale_color_viridis_c() +
    coord_cartesian(xlim = c(minlon - lonbuffer, maxlon + lonbuffer), 
                    ylim = c(minlat - latbuffer, maxlat + latbuffer)) +
    labs(x = "Longitude",
            y = "Latitude",
            colour = "PM2.5 Prediction (µg/m\u00B3)",
            title = "CTM-Based PM2.5 Estimate") +
    theme(legend.position = "bottom",
          legend.direction = "horizontal")

stage4cplt <- monitor_pm25_with_aod |>
    left_join(aod_fit_cv,
              by = c("time_id" = "time.id", 
                     "space_id" = "space.id", 
                     "spacetime_id" = "spacetime.id")) |>
    filter(date == date_use) |>
    ggplot(aes(x = longitude, y = latitude, colour = estimate)) +
    geom_polygon(data = ca_map, 
                 aes(x = long, y = lat, group = group),
                 fill = NA, color = "black") +
    geom_point(size = size_use) +
    scale_color_viridis_c() +
    coord_cartesian(xlim = c(minlon - lonbuffer, maxlon + lonbuffer), 
                    ylim = c(minlat - latbuffer, maxlat + latbuffer)) +
    labs(x = "Longitude",
         y = "Latitude",
         colour = "PM2.5 Estimate (µg/m\u00B3)",
         title = "AOD-Based PM2.5 Estimate") +
    theme(legend.position = "bottom",
          legend.direction = "horizontal")

ensemble_weights <- 1 / (exp(-ensemble_fit$q[, 2:ncol(ensemble_fit$q)]) + 1)
ensemble_weights <- apply(ensemble_weights, 1, mean)
ensemble_fit_post <- data.frame(ensemble_weights = ensemble_weights,
                                space_id = ensemble_fit$q$space.id)

stage4dplt <- monitor_pm25_with_cmaq |>
    left_join(ensemble_fit_post, by = "space_id") |>
    filter(date == date_use) |>
    ggplot(aes(x = longitude, y = latitude, colour = ensemble_weights)) +
    geom_polygon(data = ca_map, 
                 aes(x = long, y = lat, group = group),
                 fill = NA, color = "black") +
    geom_point(size = size_use) +
    scale_color_viridis_c() +
    coord_cartesian(xlim = c(minlon - lonbuffer, maxlon + lonbuffer), 
                    ylim = c(minlat - latbuffer, maxlat + latbuffer)) +
    labs(x = "Longitude",
         y = "Latitude",
         colour = "Weight Estimate",
         title = "Ensemble Weight Estimate") +
    theme(legend.position = "bottom",
          legend.direction = "horizontal")


stage4plt <- (stage4aplt + stage4bplt + stage4cplt + stage4dplt) +
    plot_layout(ncol = 2, byrow = T) +
    plot_annotation(tag_levels = "A")

scale_factor <- 0.6
ggsave(
    "../output/figures/stage4.png", 
    stage4plt, 
    width = 11 * scale_factor, 
    height = 12 * scale_factor,
    dpi = 600
)


######################
### Stage 5/6 Plot ###
######################




weights_w_locs <- weight_preds$locations
weights_tr <- 1 / (exp(-weight_preds$q) + 1)
weights_w_locs$weights <- apply(weights_tr, 1, mean)


s56_cplt <- weights_w_locs |>
    left_join(cmaq_for_predictions |>
                distinct(space_id, longitude, latitude), 
            by = c("space.id" = "space_id")) |>
    ggplot(aes(x = longitude, y = latitude, colour = weights)) +
    geom_polygon(data = ca_map, 
                 aes(x = long, y = lat, group = group),
                 fill = NA, color = "black") +
    geom_point(size = .0001) +
    labs(x = "Longitude",
         y = "Latitude",
         colour = "Weight Estimate",
         title = "Ensemble Weight") +
    scale_color_viridis_c() +
    coord_cartesian(xlim = c(minlon - lonbuffer, maxlon + lonbuffer), 
                    ylim = c(minlat - latbuffer, maxlat + latbuffer)) +
    theme(legend.position = "bottom",
          legend.direction = "horizontal")




locs <- distinct(cmaq_for_predictions[, c("space_id", "latitude", "longitude")])
dates <- distinct(cmaq_for_predictions[, c("time_id", "date")])

full_results <- cmaq_pred
full_results$estimate <- results$ensemble.estimate
full_results$sd <- results$ensemble.sd

full_results <- full_results |>
    left_join(locs, by = c("space.id" = "space_id")) |>
    left_join(dates, by = c("time.id" = "time_id"))

s56_dplt <- full_results |>
    filter(date == date_use) |>
    ggplot(aes(x = longitude, y = latitude, colour = estimate)) +
    geom_polygon(data = ca_map, 
                 aes(x = long, y = lat, group = group),
                 fill = NA, color = "black") +
    geom_point(size = .0001) +
    labs(x = "Longitude",
         y = "Latitude",
         color = "PM2.5 Estimate (µg/m\u00B3)",
         title = "Ensemble Mean") +
    scale_color_viridis_c() +
    coord_cartesian(xlim = c(minlon - lonbuffer, maxlon + lonbuffer), 
                    ylim = c(minlat - latbuffer, maxlat + latbuffer)) +
    theme(legend.position = "bottom",
          legend.direction = "horizontal")

s56_eplt <- full_results |>
    filter(date == date_use) |>
    ggplot(aes(x = longitude, y = latitude, colour = sd)) +
    geom_polygon(data = ca_map, 
                 aes(x = long, y = lat, group = group),
                 fill = NA, color = "black") +
    geom_point(size = .0001) +
    labs(x = "Longitude",
         y = "Latitude",
         color = "PM2.5 SD (µg/m\u00B3)",
         title = "Ensemble SD") +
    scale_color_viridis_c() +
    coord_cartesian(xlim = c(minlon - lonbuffer, maxlon + lonbuffer), 
                    ylim = c(minlat - latbuffer, maxlat + latbuffer)) +
    theme(legend.position = "bottom",
          legend.direction = "horizontal")



s56plt <- (s56_cplt + s56_dplt + s56_eplt) +
    plot_layout(ncol = 3, byrow = T) +
    plot_annotation(tag_levels = "A")


scale_factor <- 0.6
ggsave(
    "../output/figures/stage56.png", 
    s56plt, 
    width = 16 * scale_factor,
    height = 6 * scale_factor,
    dpi = 600
)


#########################
#######CV plot###########
#########################


cv_id_cmaq_ord <- ensembleDownscaleR::create_cv(
    time.id = monitor_pm25_with_cmaq$time_id, 
    space.id = monitor_pm25_with_cmaq$space_id,
    spacetime.id = monitor_pm25_with_cmaq$spacetime_id,
    type = "ordinary",
    num.folds = 5
)

cv_id_cmaq_spat <- ensembleDownscaleR::create_cv(
    time.id = monitor_pm25_with_cmaq$time_id, 
    space.id = monitor_pm25_with_cmaq$space_id,
    spacetime.id = monitor_pm25_with_cmaq$spacetime_id,
    type = "spatial",
    num.folds = 5
)

cv_id_cmaq_spatclus <- ensembleDownscaleR::create_cv(
    time.id = monitor_pm25_with_cmaq$time_id, 
    space.id = monitor_pm25_with_cmaq$space_id,
    spacetime.id = monitor_pm25_with_cmaq$spacetime_id,
    type = "spatial_clustered",
    coords = monitor_pm25_with_cmaq[, c("x", "y")],
    num.folds = 5
)


cv_id_cmaq_spatbuff <- ensembleDownscaleR::create_cv(
    time.id = monitor_pm25_with_cmaq$time_id, 
    space.id = monitor_pm25_with_cmaq$space_id,
    spacetime.id = monitor_pm25_with_cmaq$spacetime_id,
    type = "spatial_buffered",
    coords = monitor_pm25_with_cmaq[, c("x", "y")],
    buffer.size = 30,
    num.folds = 5
)


full_ord <- monitor_pm25_with_cmaq
full_ord$cv_id <- cv_id_cmaq_ord$cv.id
full_ord$cv_type <- "Ordinary"
full_spat <- monitor_pm25_with_cmaq
full_spat$cv_id <- cv_id_cmaq_spat$cv.id
full_spat$cv_type <- "Spatial"
full_spatclus <- monitor_pm25_with_cmaq
full_spatclus$cv_id <- cv_id_cmaq_spatclus$cv.id
full_spatclus$cv_type <- "Spatial Clustered"
full_spatbuff <- monitor_pm25_with_cmaq
full_spatbuff$cv_id <- cv_id_cmaq_spatbuff$cv.id
full_spatbuff$cv_id <- as.character(full_spatbuff$cv_id)
full_spatbuff$cv_id <- ifelse(full_spatbuff$cv_id == "1", "1", "2-5")
full_spatbuff$cv_id <- ifelse(cv_id_cmaq_spatbuff$drop.matrix[, 1] == 1, "Dropped", full_spatbuff$cv_id)
full_spatbuff$cv_type <- "Spatial Buffered"


date_counts <- table(full_ord$date)
date_for_cv <- sample(names(date_counts[which(date_counts >= 59)]), 4)
full_cv <- rbind(full_ord, full_spat, full_spatclus, full_spatbuff) |>
    filter(date %in% date_for_cv)


cv_ex_plt <- full_cv |>
    mutate(cv_id = factor(cv_id, levels = c("1", "2", "3", "4", "5", "2-5", "Dropped"))) |>
    ggplot(aes(x = longitude, y = latitude, color = cv_id)) +
    geom_polygon(data = ca_map, 
                 aes(x = long, y = lat, group = group),
                 fill = NA, color = "black") +
    geom_point(size = 2) +
    facet_grid(cv_type ~ date) +
    scale_color_viridis_d() +
    coord_cartesian(xlim = c(minlon - lonbuffer, maxlon + lonbuffer), 
                    ylim = c(minlat - latbuffer, maxlat + latbuffer)) +
    labs(x = "Longitude",
         y = "Latitude",
         color = "CV Assignment")

scale_factor <- 0.8
ggsave(
    "../output/figures/cv.png", 
    cv_ex_plt, 
    width = 11 * scale_factor,
    height = 9 * scale_factor,
    dpi = 600
)

####################
### Results Table###
####################

pred_obs_full <- ensemble_preds_at_observations |>
    left_join(cmaq_fit_cv[, c("time.id", "space.id", "spacetime.id", "obs","estimate", "sd")],
              by = c("time.id", "space.id", "spacetime.id")) |>
    mutate(cmaq.estimate = estimate,
           cmaq.sd = sd) |>
    select(-estimate, -sd) |>
    left_join(aod_fit_cv[, c("time.id", "space.id", "spacetime.id", "estimate", "sd")],
              by = c("time.id", "space.id", "spacetime.id")) |>
    mutate(aod.estimate = estimate,
           aod.sd = sd) |>
    select(-estimate, -sd) |>
    filter(!is.na(ensemble.estimate),
           !is.na(cmaq.estimate),
           !is.na(aod.estimate)) |>
    pivot_longer(cols = c(ensemble.estimate, cmaq.estimate, aod.estimate, 
                          ensemble.sd, cmaq.sd, aod.sd), 
                 names_to = c("type", ".value"),
                 names_pattern = "(.*)\\.(.*)"
  )

pred_obs_full_spat <- ensemble_preds_at_observations_spat |>
    left_join(cmaq_fit_cv_spat[, c("time.id", "space.id", "spacetime.id", "obs","estimate", "sd")],
              by = c("time.id", "space.id", "spacetime.id")) |>
    mutate(cmaq.estimate = estimate,
           cmaq.sd = sd) |>
    select(-estimate, -sd) |>
    left_join(aod_fit_cv_spat[, c("time.id", "space.id", "spacetime.id", "estimate", "sd")],
              by = c("time.id", "space.id", "spacetime.id")) |>
    mutate(aod.estimate = estimate,
           aod.sd = sd) |>
    select(-estimate, -sd) |>
    filter(!is.na(ensemble.estimate),
           !is.na(cmaq.estimate),
           !is.na(aod.estimate)) |>
    pivot_longer(cols = c(ensemble.estimate, cmaq.estimate, aod.estimate, 
                          ensemble.sd, cmaq.sd, aod.sd), 
                 names_to = c("type", ".value"),
                 names_pattern = "(.*)\\.(.*)"
  )

pred_obs_full_spatclust <- ensemble_preds_at_observations_spatclust |>
    left_join(cmaq_fit_cv_spatclust[, c("time.id", "space.id", "spacetime.id", "obs","estimate", "sd")],
              by = c("time.id", "space.id", "spacetime.id")) |>
    mutate(cmaq.estimate = estimate,
           cmaq.sd = sd) |>
    select(-estimate, -sd) |>
    left_join(aod_fit_cv_spatclust[, c("time.id", "space.id", "spacetime.id", "estimate", "sd")],
              by = c("time.id", "space.id", "spacetime.id")) |>
    mutate(aod.estimate = estimate,
           aod.sd = sd) |>
    select(-estimate, -sd) |>
    filter(!is.na(ensemble.estimate),
           !is.na(cmaq.estimate),
           !is.na(aod.estimate)) |>
    pivot_longer(cols = c(ensemble.estimate, cmaq.estimate, aod.estimate, 
                          ensemble.sd, cmaq.sd, aod.sd), 
                 names_to = c("type", ".value"),
                 names_pattern = "(.*)\\.(.*)"
  )

pred_obs_full_spatbuff3 <- ensemble_preds_at_observations_spatbuff3 |>
    left_join(cmaq_fit_cv_spatbuff3[, c("time.id", "space.id", "spacetime.id", "obs","estimate", "sd")],
              by = c("time.id", "space.id", "spacetime.id")) |>
    mutate(cmaq.estimate = estimate,
           cmaq.sd = sd) |>
    select(-estimate, -sd) |>
    left_join(aod_fit_cv_spatbuff3[, c("time.id", "space.id", "spacetime.id", "estimate", "sd")],
              by = c("time.id", "space.id", "spacetime.id")) |>
    mutate(aod.estimate = estimate,
           aod.sd = sd) |>
    select(-estimate, -sd) |>
    filter(!is.na(ensemble.estimate),
           !is.na(cmaq.estimate),
           !is.na(aod.estimate)) |>
    pivot_longer(cols = c(ensemble.estimate, cmaq.estimate, aod.estimate, 
                          ensemble.sd, cmaq.sd, aod.sd), 
                 names_to = c("type", ".value"),
                 names_pattern = "(.*)\\.(.*)"
  )

pred_obs_full_spatbuff7 <- ensemble_preds_at_observations_spatbuff7 |>
    left_join(cmaq_fit_cv_spatbuff7[, c("time.id", "space.id", "spacetime.id", "obs","estimate", "sd")],
              by = c("time.id", "space.id", "spacetime.id")) |>
    mutate(cmaq.estimate = estimate,
           cmaq.sd = sd) |>
    select(-estimate, -sd) |>
    left_join(aod_fit_cv_spatbuff7[, c("time.id", "space.id", "spacetime.id", "estimate", "sd")],
              by = c("time.id", "space.id", "spacetime.id")) |>
    mutate(aod.estimate = estimate,
           aod.sd = sd) |>
    select(-estimate, -sd) |>
    filter(!is.na(ensemble.estimate),
           !is.na(cmaq.estimate),
           !is.na(aod.estimate)) |>
    pivot_longer(cols = c(ensemble.estimate, cmaq.estimate, aod.estimate, 
                          ensemble.sd, cmaq.sd, aod.sd), 
                 names_to = c("type", ".value"),
                 names_pattern = "(.*)\\.(.*)"
  )

pred_obs_full$cv <- "Ordinary"
pred_obs_full_spat$cv <- "Spatial"
pred_obs_full_spatclust$cv <- "Spatial Clustered"
pred_obs_full_spatbuff3$cv <- "Spatial Buffered (0.3 Corr)"
pred_obs_full_spatbuff7$cv <- "Spatial Buffered (0.7 Corr)"

pred_obs_all <- rbind(
    pred_obs_full, 
    pred_obs_full_spat,
    pred_obs_full_spatclust,
    pred_obs_full_spatbuff3,
    pred_obs_full_spatbuff7
  )


pred_obs_full_metrics_table <- pred_obs_all |>
    mutate(lower = estimate - 1.96 * sd,
           upper = estimate + 1.96 * sd) |>
    group_by(cv, type) |>
    summarise(
        rmse = sqrt(mean((estimate - obs)^2)),
        R2 = 1 - sum((obs - estimate)^2) / sum((obs - mean(obs))^2),
        avg_sd = mean(sd),
        coverage = mean((lower <= obs) & (upper >= obs))
    ) |>
    mutate(type = case_when(
        type == "ensemble" ~ "Ensemble Model",
        type == "cmaq" ~ "CMAQ-Based Model",
        type == "aod" ~ "AOD-Based Model"
        )
    ) |>
    rename("Cross-Validation" = cv,
           "Model" = type,
           "RMSE" = rmse,
           "R^2" = R2,
           "Average Posterior SD" = avg_sd,
           "Coverage of 95% PI" = coverage) |>
    knitr::kable("latex", digits = 3)




writeLines(
    pred_obs_full_metrics_table, 
    "../output/figures/pred_obs_full_metrics_table.tex"
)


###############
### Runtime ###
###############

minutestime <- runtime['elapsed'] / 60
hourstime <- minutestime / 60









