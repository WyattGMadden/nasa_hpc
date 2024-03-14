#get CTM, grid.info, OBS, OBS.info
#process observations
obs <- read.csv("../../data/howard_uploaded/wildfire CA_OR.csv")
names(obs) <- tolower(names(obs))
obs <- obs[, names(obs)[2:ncol(obs)]]  #n
obs$x <- obs$cen_x / 1e3
obs$y <- obs$cen_y / 1e3
obs <- obs[, !(names(obs) %in% c("cen_x", "cen_y"))]

obs$date <- as.Date(obs$date)
obs$space_id <- as.numeric(as.factor(obs$maiac_id))
obs$time_id <- as.numeric(as.factor(as.numeric(obs$date)))
obs$spacetime_id <- 1

str(obs)


saveRDS(obs, "../../data/created/obs.rds")


pred_file_locs <- list.files("../../data/howard_uploaded/predictor_July_2018/", full.names = TRUE)
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


preds$date <- as.Date(preds$date)
preds$space_id <- as.numeric(as.factor(preds$maiac_id))

#join on time_id from obs by date
preds$time_id <- as.numeric(factor(as.numeric(preds$date), 
                                      levels = levels(as.factor(as.numeric(obs$date)))))
preds$spacetime_id <- 1

saveRDS(preds, "../../data/created/preds.rds")





#get cv objects
cv_types <- c("ordinary", 
              "spatial", 
              "spatial_clustered", 
              "spatial_buffered_35",
              "spatial_buffered_100")

for (cv_type in cv_types) {

    type <- cv_type
    buffer.size <- NULL

    if (!(cv_type %in% cv_types[1:3])) {
        type <- "spatial_buffered"
        buffer.size <- as.numeric(substr(cv_type, 18, nchar(cv_type)))
    }
    cv_out <- grmbayes::create_cv(space.id = obs$space_id, 
                                  time.id = obs$time_id,
                                  type = type,
                                  coords = obs[, c("x", "y")],
                                  buffer.size = buffer.size)
    saveRDS(cv_out, paste0("../../data/created/cv_objects/", cv_type, ".rds"))

}

#get job params
matern.nu <- c(0.5, 1.5, 2.5)
job_params <- expand.grid(matern.nu = matern.nu,
                          cv = cv_types,
                          stringsAsFactors = FALSE)

write.csv(job_params, "../../data/created/job_params.csv", row.names = FALSE, quote = FALSE)




# Plot
locs <- read.csv("../../data/howard_uploaded/CA_OR_MAIAC_Grid_wLatLon.csv", check.names = F)
names(locs) <- c("oid", tolower(names(locs)[2:ncol(locs)]))

states_map <- map_data("state")
california_map <- subset(states_map, region == "california")

#left join locs on obs by maiac_id
obs <- merge(obs, 
             locs[, c("longitude", "latitude", "maiac_id")],
             by.x = "maiac_id", 
             by.y = "maiac_id", 
             all.x = TRUE)

#left join locs on preds by maiac_id
preds <- merge(preds, 
               locs[, c("longitude", "latitude", "maiac_id")],
               by.x = "maiac_id", 
               by.y = "maiac_id", 
               all.x = TRUE)

obs |>
    ggplot() +
    geom_polygon(data = california_map,
                 aes(x = long, y = lat, group = group),
                 fill = "white",
                 color = "black"
    ) +
    geom_point(aes(y = latitude, x = longitude))

# Plot
preds |>
    ggplot() +
    geom_polygon(data = california_map,
                 aes(x = long, y = lat, group = group),
                 fill = "white",
                 color = "black"
    ) +
    geom_point(aes(y = latitude, x = longitude, colour = aod.final))











