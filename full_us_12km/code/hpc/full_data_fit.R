full_grid_run <- function(matern.nu, cv) {
    obs_full_grid <- readRDS("../../full_grid/data/obs_full_grid.rds")
    cv_object <- readRDS(paste0("../../full_grid/data/cv_id_obs_full_grid_", cv, ".rds"))

    n_iter <- 10000
    burn <- 1000
    thin <- 4
    discrete.theta.alpha.values <- seq(10, 60, 2.5)       
    discrete.theta.beta.values <- seq(10, 60, 2.5)       
    if (matern.nu %in% c(1.5, 2.5)) {
        matern_scale = 0.1
        discrete.theta.alpha.values <- discrete.theta.alpha.values * matern_scale
        discrete.theta.beta.values <- discrete.theta.beta.values * matern_scale
    }

    time_fit <- system.time({
    ctm_fit <- grmbayes::grm(Y = obs_full_grid$pm_aqs,
                             X = obs_full_grid$pm25_tot_ncar,
                             n.iter = n_iter,
                             burn = burn,
                             thin = thin,
                             nngp = T,
                             covariance = "matern",
                             matern.nu = matern.nu,
                             discrete.theta.gibbs = F,
                             discrete.theta.alpha.values = discrete.theta.alpha.values,
                             discrete.theta.beta.values = discrete.theta.beta.values,
                             coords = obs_full_grid[, c("x", "y")],
                             space.id = obs_full_grid$space_id,
                             time.id = obs_full_grid$time_id,
                             spacetime.id = obs_full_grid$spacetime_id,
                             verbose.iter = 1000)
    })
    time_fit_cv <- system.time({
    ctm_fit_cv <- grmbayes::grm_cv(Y = obs_full_grid$pm_aqs,
                             X = obs_full_grid$pm25_tot_ncar,
                             cv.object = cv_object,
                             n.iter = n_iter,
                             burn = burn,
                             thin = thin,
                             nngp = T,
                             covariance = "matern",
                             matern.nu = matern.nu,
                             discrete.theta.gibbs = F,
                             discrete.theta.alpha.values = discrete.theta.alpha.values,
                             discrete.theta.beta.values = discrete.theta.beta.values,
                             coords = obs_full_grid[, c("x", "y")],
                             space.id = obs_full_grid$space_id,
                             time.id = obs_full_grid$time_id,
                             spacetime.id = obs_full_grid$spacetime_id,
                             verbose.iter = 1000)
    })


    output <- list(ctm_fit = ctm_fit, 
                   ctm_fit_cv = ctm_fit_cv, 
                   matern.nu = matern.nu,
                   cv = cv,
                   time_fit = time_fit,
                   time_fit_cv = time_fit_cv)
    saveRDS(output, 
            paste0("../output/data/full_grid/full_grid_", 
                   matern.nu,
                   "_", 
                   cv, 
                   ".RDS"))
}
