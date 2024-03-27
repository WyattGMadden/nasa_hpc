full_fit <- function(matern.nu, cv) {

    obs <- readRDS("../../data/created/obs.rds")
    cv_object <- readRDS(paste0("../../data/created/cv_objects/", cv, ".rds"))

    n_iter <- 10000
    burn <- 2000
    thin <- 4


    discrete.theta.alpha.values <- seq(5, 75, 5)       
    discrete.theta.beta.values <- seq(5, 75, 5)       

    time_fit <- system.time({
    ctm_fit <- grmbayes::grm(Y = obs$pm25,
                             X = obs$aod.final,
                             L = obs[, c("elevation", "population")],
                             M = obs[, c("cloud", "v_wind", "hpbl",
                                         "u_wind", "short_rf", "humidity_2m")],
                             n.iter = n_iter,
                             burn = burn,
                             thin = thin,
                             nngp = T,
                             covariance = "matern",
                             matern.nu = matern.nu,
                             theta.alpha.a = 0.005,
                             theta.alpha.b = 0.005,
                             theta.beta.a = 0.005,
                             theta.beta.b = 0.005,
                             theta.alpha.init = 50,
                             theta.beta.init = 50,
                             theta.alpha.tune = 0.2,
                             theta.beta.tune = 0.2,
                             tau.alpha.a = 0.005,
                             tau.alpha.b = 0.005,
                             tau.beta.a = 0.005,
                             tau.beta.b = 0.005,
                             tau.alpha.tune = 0.05,
                             tau.beta.tune = 0.05,
                             discrete.theta.gibbs = F,
                             discrete.theta.alpha.values = discrete.theta.alpha.values,
                             discrete.theta.beta.values = discrete.theta.beta.values,
                             coords = obs[, c("x", "y")],
                             space.id = obs$space_id,
                             time.id = obs$time_id,
                             spacetime.id = obs$spacetime_id,
                             verbose.iter = 1000)
    })
    time_fit_cv <- system.time({
    ctm_fit_cv <- grmbayes::grm_cv(Y = obs$pm25,
                                   L = obs[, c("elevation", "population")],
                                   M = obs[, c("cloud", "v_wind", "hpbl",
                                               "u_wind", "short_rf", "humidity_2m")],
                                   X = obs$aod.final,
                                   cv.object = cv_object,
                                   n.iter = n_iter,
                                   burn = burn,
                                   thin = thin,
                                   nngp = T,
                                   covariance = "matern",
                                   matern.nu = matern.nu,
                                     theta.alpha.a = 0.005,
                                     theta.alpha.b = 0.005,
                                     theta.beta.a = 0.005,
                                     theta.beta.b = 0.005,
                                     theta.alpha.init = 50,
                                     theta.beta.init = 50,
                                     theta.alpha.tune = 0.2,
                                     theta.beta.tune = 0.2,
                                     tau.alpha.a = 0.005,
                                     tau.alpha.b = 0.005,
                                     tau.beta.a = 0.005,
                                     tau.beta.b = 0.005,
                                     tau.alpha.tune = 0.05,
                                     tau.beta.tune = 0.05,
                                   discrete.theta.gibbs = F,
                                   discrete.theta.alpha.values = discrete.theta.alpha.values,
                                   discrete.theta.beta.values = discrete.theta.beta.values,
                                   coords = obs[, c("x", "y")],
                                   space.id = obs$space_id,
                                   time.id = obs$time_id,
                                   spacetime.id = obs$spacetime_id,
                                   verbose.iter = 1000)
    })


    output <- list(ctm_fit = ctm_fit, 
                   ctm_fit_cv = ctm_fit_cv, 
                   matern.nu = matern.nu,
                   cv = cv,
                   time_fit = time_fit,
                   time_fit_cv = time_fit_cv)
    saveRDS(output, 
            paste0("~/../../projects/hhchang/wmadden/nasa_hpc/ca_1km_w_cov/output/results/prelim_fits/fit_",
                   matern.nu,
                   "_", 
                   cv, 
                   ".RDS"))
}
