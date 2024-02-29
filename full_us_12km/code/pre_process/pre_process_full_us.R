set.seed(1001001)

#get CTM, grid.info, OBS, OBS.info
#process observations
load("../../data/howard_uploaded/Grid_PM25.RData")

obs <- OBS
names(obs) <- tolower(names(obs))

obs$date <- as.Date(obs$date)
obs$space_id <- as.numeric(as.factor(obs$aqs_site_id))
obs$time_id <- as.numeric(as.factor(as.numeric(obs$date)))
obs$spacetime_id <- as.numeric(substr(obs$date, 6, 7))


#remove space ids that don't show up in every spacetime
unique_space_spacetime_ids <- unique(paste(obs$space_id, obs$spacetime_id, sep = "-"))
all_possible_ids <- paste(rep(1:max(obs$space_id), times = max(obs$spacetime_id)),
                          rep(1:max(obs$spacetime_id), each = max(obs$space_id)),
                          sep = "-")

not_present <- all_possible_ids[!(all_possible_ids %in% unique_space_spacetime_ids)]
offending_space_ids <- sub("-.*$", "", not_present)
length(unique(obs$space_id))

dim(obs)
obs <- obs[!(obs$space_id %in% as.integer(offending_space_ids)), ]
obs$space_id <- as.numeric(as.factor(obs$aqs_site_id))
obs$time_id <- as.numeric(as.factor(as.numeric(obs$date)))
obs$spacetime_id <- as.numeric(substr(obs$date, 6, 7))


saveRDS(obs, "../../data/created/obs.rds")


#process preds

grid.info <- grid.info
names(grid.info) <- tolower(names(grid.info))
grid.info <- grid.info[, c("grid_cell", "x", "y")]

preds <- CTM
names(preds) <- tolower(names(preds))

preds$date <- as.Date(preds$date)
preds <- preds[preds$date %in% obs$date, ]
preds$space_id <- preds$grid_cell
preds$time_id <- as.numeric(factor(as.numeric(preds$date),
                                   levels = levels(as.factor(as.numeric(obs$date)))))
preds$spacetime_id <- as.numeric(substr(preds$date, 6, 7))

#left join grid.info on preds
preds <- merge(preds, 
               grid.info, 
               by = "grid_cell",
               all.x = TRUE)

saveRDS(preds, "../../data/created/preds.rds")



#cv objects
#read prelim results for correlated calculation for spatial buffering
fit_mat <- readRDS("../../data/created/prelim_fits.rds")



others_out <- lapply(fit_mat,
                 function(x) {
                     fit <- x$others
                     fit$matern_nu <- x$matern.nu
                     fit$iter <- 1:nrow(fit)
                     return(fit)
                    }
                 ) |>
    (\(.) Reduce(rbind, .))()

###output correlation###
#based on max(theta_alpha, theta_beta), nu = 0.5
#correlation of 0.7 and 0.3

mean_theta_alpha <- mean(others_out$theta.alpha[others_out$matern_nu == 0.5])
mean_theta_beta <- mean(others_out$theta.beta[others_out$matern_nu == 0.5])
mean_theta <- max(c(mean_theta_alpha, mean_theta_beta))

buffer_0.8 <- - mean_theta * log(0.7)
buffer_0.5 <- - mean_theta * log(0.3)


#get cv objects
cv_types <- c("ordinary", 
              "spatial", 
              "spatial_clustered", 
              paste0("spatial_buffered_", round(buffer_0.8, 2)),
              paste0("spatial_buffered_", round(buffer_0.5, 2)))

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

write.csv(job_params, 
          "../../data/created/job_params.csv", 
          row.names = FALSE,
          quote = F)








