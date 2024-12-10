data_loc <- "../data/"


locs <- read.csv(paste0(data_loc, "CA_OR_MAIAC_Grid_wLatLon.csv"), check.names = F)
names(locs) <- c("oid", tolower(names(locs)[2:ncol(locs)]))


#get CTM, grid.info, OBS, OBS.info
#process observations
obs <- read.csv(paste0(data_loc, "wildfire CA_OR.csv"))
names(obs) <- tolower(names(obs))
obs <- obs[, names(obs)[2:ncol(obs)]]  #n
obs$x <- obs$cen_x / 1e3
obs$y <- obs$cen_y / 1e3
obs <- obs[, !(names(obs) %in% c("cen_x", "cen_y"))]
obs <- obs[obs$state.y == "California", ]


obs$date <- as.Date(obs$date)


# rename some variables
obs$aod <- obs$aod.final
obs$cmaq <- obs$pm_idw
obs <- obs[, c("date", 
               "maiac_id",
               "pm25",
               "aod", 
               "cmaq",
               "gap.fill",
               "elevation",
               "population",
               "cloud",
               "v_wind",
               "u_wind",
               "hpbl",
               "short_rf",
               "humidity_2m",
               "x", 
               "y")]


#left join locs on obs by maiac_id
obs <- merge(obs, 
             locs[, c("longitude", "latitude", "maiac_id")],
             by.x = "maiac_id", 
             by.y = "maiac_id", 
             all.x = TRUE)

lat_max_bound <- 35.70
lat_min_bound <- 32
long_max_bound <- -115.75
long_min_bound <- -120.5

# filter for 60 monitors in LA region
obs_la60 <- obs[obs$latitude > lat_min_bound & 
                obs$latitude < lat_max_bound & 
                obs$longitude < long_max_bound & 
                obs$longitude > long_min_bound, ]

# remove monitors with less than 10 observations so cv works
filter_space_id <- as.integer(names(which(table(obs_la60$maiac_id) < 10)))
obs_la60 <- obs_la60[!(obs_la60$maiac_id %in% filter_space_id), ]

length(unique(obs_la60$maiac_id))

# renumber space_id/time_id
obs_la60$space_id <- as.numeric(as.factor(obs_la60$maiac_id))
obs_la60$time_id <- as.numeric(as.factor(as.numeric(obs_la60$date)))
obs_la60$spacetime_id <- 1


library(ggplot2)
states_map <- map_data("state")
california_map <- subset(states_map, region == "california")
temp <- obs_la60 |>
    ggplot() +
    geom_polygon(data = california_map,
                 aes(x = long, y = lat, group = group),
                 fill = "white",
                 color = "black") +
    geom_point(aes(y = latitude, x = longitude))

monitor_pm25_with_cmaq <- obs_la60[, c("time_id",
                                       "space_id",
                                       "spacetime_id",
                                       "pm25",
                                       "cmaq",
                                       "elevation",
                                       "population",
                                       "cloud",
                                       "v_wind",
                                       "hpbl",
                                       "u_wind",
                                       "short_rf",
                                       "humidity_2m",
                                       "date",
                                       "x",
                                       "y",
                                       "longitude",
                                       "latitude")]
monitor_pm25_with_aod <- obs_la60[obs_la60$gap.fill == 0, 
                                  c("time_id",
                                    "space_id",
                                    "spacetime_id",
                                    "pm25",
                                    "aod",
                                    "elevation",
                                    "population",
                                    "cloud",
                                    "v_wind",
                                    "hpbl",
                                    "u_wind",
                                    "short_rf",
                                    "humidity_2m",
                                    "date",
                                    "x",
                                    "y",
                                    "longitude",
                                    "latitude")]


table(monitor_pm25_with_cmaq$space_id)
saveRDS(monitor_pm25_with_cmaq, "../data/monitor_pm25_with_cmaq.rds")
saveRDS(monitor_pm25_with_aod, "../data/monitor_pm25_with_aod.rds")

unique(monitor_pm25_with_cmaq$space_id)
unique(monitor_pm25_with_aod$space_id)

#process prediction data
pred_file_locs <- list.files("../../data/howard_uploaded/predictor_July_2018/", full.names = TRUE)

pred_file_locs <- list.files(paste0(data_loc, "predictor_July_2018"), full.names = TRUE)
pred_file_process <- function(x) {
    pred <- readRDS(x)
    names(pred) <- tolower(names(pred))
    pred$date <- as.Date(substr(x, nchar(x) - 13, nchar(x) - 4))
    print(paste("read in", unique(pred$date)))
    return(pred)
}

preds <- lapply(pred_file_locs, pred_file_process)
preds <- do.call(rbind, preds)
preds$x <- preds$cen_x / 1e3
preds$y <- preds$cen_y / 1e3
preds <- preds[, !(names(preds) %in% c("cen_x", "cen_y"))]
preds <- preds[preds$stusps == "CA", ]


preds$date <- as.Date(preds$date)


# rename some variables
preds$aod <- preds$aod.final
preds$cmaq <- preds$pm_idw
preds <- preds[, c("date", 
                   "maiac_id",
                   "aod", 
                   "cmaq",
                   "gap.fill",
                   "elevation",
                   "population",
                   "cloud",
                   "v_wind",
                   "u_wind",
                   "hpbl",
                   "short_rf",
                   "humidity_2m",
                   "x", 
                   "y")]


preds <- merge(preds, 
               locs[, c("longitude", "latitude", "maiac_id")],
               by.x = "maiac_id", 
               by.y = "maiac_id", 
               all.x = TRUE)


# filter LA region, first week
preds_la60 <- preds[preds$latitude > lat_min_bound & 
                    preds$latitude < lat_max_bound & 
                    preds$longitude < long_max_bound & 
                    preds$longitude > long_min_bound, ]

# renumber space_id/time_id
preds_la60$space_id <- as.numeric(as.factor(preds_la60$maiac_id))

#join on time_id from obs by date
preds_la60$time_id <- as.numeric(factor(as.numeric(preds_la60$date), 
                                      levels = levels(as.factor(as.numeric(preds_la60$date)))))
preds_la60$spacetime_id <- 1


full_cmaq <- preds_la60[, c("time_id",
                            "space_id",
                            "spacetime_id",
                            "cmaq",
                            "elevation",
                            "population",
                            "cloud",
                            "v_wind",
                            "hpbl",
                            "u_wind",
                            "short_rf",
                            "humidity_2m",
                            "date",
                            "x",
                            "y",
                            "longitude",
                            "latitude")]
full_aod <- preds_la60[preds_la60$gap.fill == 0, 
                       c("time_id",
                         "space_id",
                         "spacetime_id",
                         "aod",
                         "elevation",
                         "population",
                         "cloud",
                         "v_wind",
                         "hpbl",
                         "u_wind",
                         "short_rf",
                         "humidity_2m",
                         "date",
                         "x",
                         "y",
                         "longitude",
                         "latitude")]

saveRDS(full_cmaq, "../data/cmaq_for_predictions.rds")
saveRDS(full_aod, "../data/aod_for_predictions.rds")

