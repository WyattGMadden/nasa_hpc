full_us_run <- function(matern.nu, cv) {
    #temp
#    n_iter <- 100
#    burn <- 10
#    thin <- 4
#    cv <- "ordinary"
#    matern.nu <- 1.5

    obs_full_us <- readRDS("../../data/created/obs.rds")
    cv_object <- readRDS(paste0("../../data/created/cv_objects/", cv, ".rds"))
    length(unique(obs_full_us$space_id))

    n_iter <- 10000
    burn <- 2000
    thin <- 4


    discrete.theta.alpha.values <- seq(10, 60, 2.5)       
    discrete.theta.beta.values <- seq(10, 60, 2.5)       
#    if (matern.nu %in% c(1.5, 2.5)) {
#        matern_scale = 0.1
#        discrete.theta.alpha.values <- discrete.theta.alpha.values * matern_scale
#        discrete.theta.beta.values <- discrete.theta.beta.values * matern_scale
#    }

    time_fit <- system.time({
    ctm_fit <- grmbayes::grm(Y = obs_full_us$pm_aqs,
                             X = obs_full_us$pm25_tot_ncar,
                             n.iter = n_iter,
                             burn = burn,
                             thin = thin,
                             nngp = T,
                             covariance = "matern",
                             matern.nu = matern.nu,
                             discrete.theta.gibbs = F,
                             discrete.theta.alpha.values = discrete.theta.alpha.values,
                             discrete.theta.beta.values = discrete.theta.beta.values,
                             coords = obs_full_us[, c("x", "y")],
                             space.id = obs_full_us$space_id,
                             time.id = obs_full_us$time_id,
                             spacetime.id = obs_full_us$spacetime_id,
                             verbose.iter = 1000)
    })
    time_fit_cv <- system.time({
    ctm_fit_cv <- grmbayes::grm_cv(Y = obs_full_us$pm_aqs,
                             X = obs_full_us$pm25_tot_ncar,
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
                             coords = obs_full_us[, c("x", "y")],
                             space.id = obs_full_us$space_id,
                             time.id = obs_full_us$time_id,
                             spacetime.id = obs_full_us$spacetime_id,
                             verbose.iter = 1000)
    })


    output <- list(ctm_fit = ctm_fit, 
                   ctm_fit_cv = ctm_fit_cv, 
                   matern.nu = matern.nu,
                   cv = cv,
                   time_fit = time_fit,
                   time_fit_cv = time_fit_cv)
    saveRDS(output, 
            paste0("../output/results/full_us_fit_",
                   matern.nu,
                   "_", 
                   cv, 
                   ".RDS"))
}
