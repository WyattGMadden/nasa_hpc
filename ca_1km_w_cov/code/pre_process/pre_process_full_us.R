#get CTM, grid.info, OBS, OBS.info
#process observations
load("../../data/howard_uploaded/Grid_PM25.RData")

obs <- OBS
names(obs) <- tolower(names(obs))

obs$date <- as.Date(obs$date)
obs$space_id <- as.numeric(as.factor(obs$aqs_site_id))
obs$time_id <- as.numeric(as.factor(as.numeric(obs$date)))
obs$spacetime_id <- as.numeric(substr(obs$date, 6, 7))


saveRDS(obs, "../../data/created/obs.rds")


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

write.csv(job_params, "../../data/created/job_params.csv", row.names = FALSE)








