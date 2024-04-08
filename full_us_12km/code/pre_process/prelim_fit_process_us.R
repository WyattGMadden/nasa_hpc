dirs <- list.files("../../data/created/prelim_fit",
                   full.names = T)

all <- lapply(dirs,
              function(x) {
                  temp <- readRDS(x)
                  temp$matern_nu <- substr(x, 
                                           nchar(x) - 6, 
                                           nchar(x) - 4) |>
                    as.numeric()
                  return(temp)
              })


others_out <- lapply(all,
                 function(x) {
                     fit <- x$others
                     fit$matern_nu <- x$matern_nu
                     fit$iter <- 1:nrow(fit)
                     return(fit)
                    }
                 ) |>
    (\(.) Reduce(rbind, .))()

matern_kernal <- function(distance, theta, nu) {

    if (!(nu %in% c(0.5, 1.5, 2.5))) {
        stop("nu must be one of 0.5, 1.5, or 2.5")
    }
    
    root_2_nu_distance_theta <- sqrt(2 * nu) * distance / theta

    if (nu == 0.5) {

        kern <- exp(-root_2_nu_distance_theta)

    } else if (nu == 1.5) {

        kern <- (1 + root_2_nu_distance_theta) * exp(-root_2_nu_distance_theta)

    } else if (nu == 2.5) {
        
        kern <- (1 + root_2_nu_distance_theta + root_2_nu_distance_theta^2 / 3) * exp(-root_2_nu_distance_theta)

    }

    return(kern)
}



get_buffer_cor <- function(nu, corr, others_out) {
    others_out_tb <- others_out[others_out$iter > 200 & (others_out$iter %% 4 == 0),]

    mean_theta_alpha <- mean(others_out_tb$theta.alpha[others_out_tb$matern_nu == nu])
    mean_theta_beta <- mean(others_out_tb$theta.beta[others_out_tb$matern_nu == nu])
    mean_theta <- max(c(mean_theta_alpha, mean_theta_beta))

    #range of reasonable distances
    seq_dist <- seq(0.1, 1000, 0.1)

    #get distance for desired correlation
    buffer <- seq_dist[which.min(abs(matern_kernal(seq_dist, mean_theta, nu) - corr))]

    return(buffer)
}
buffer_params <- expand.grid(nu = seq(0.5, 2.5, 1), 
                           corr = c(0.7, 0.3))

buffer_params$distance <- 0
for (i in 1:nrow(buffer_params)) {
    buffer_params[i, "distance"] <- get_buffer_cor(buffer_params$nu[i], 
                                                   buffer_params$corr[i], 
                                                   others_out)
}

write.csv(buffer_params, "../../data/created/buffer_params.csv")




#get cv objects
obs <- readRDS("../../data/created/obs.rds")
cv_types <- c("ordinary", 
              "spatial", 
              "spatial_clustered", 
              rep("spatial_buffered", nrow(buffer_params)))
buffer.size <- c(rep(0, 3),
                 buffer_params$distance)

for (i in 1:length(cv_types)) {


    type <- cv_types[i]
    bs <- NULL
    bs_print <- NULL
    if (type == "spatial_buffered") {
        bs <- buffer.size[i]
        bs_print <- paste0("_", bs)
    }

    cv_out <- grmbayes::create_cv(space.id = obs$space_id,
                                  time.id = obs$time_id,
                                  type = type,
                                  coords = obs[, c("x", "y")],
                                  buffer.size = bs)
    saveRDS(cv_out, 
            paste0("../../data/created/cv_objects/", type, bs_print, ".rds"))

}

#get job params
matern.nu <- c(0.5, 1.5, 2.5)
cv_types_all <- list.files("../../data/created/cv_objects/")
cv_types_all <- substr(cv_types_all, 1, nchar(cv_types_all) - 4)
buffer.size
buffer_params$cv <- paste0("spatial_buffered_", buffer_params$distance)
buffer_params$matern.nu <- buffer_params$nu
job_params <- expand.grid(matern.nu = matern.nu,
                          cv = c("ordinary", 
                                 "spatial", 
                                 "spatial_clustered"),
                          stringsAsFactors = FALSE) |>
    rbind(buffer_params[, c("matern.nu", "cv")])

write.csv(job_params, 
          "../../data/created/job_params.csv", 
          row.names = FALSE, 
          quote = FALSE)

