full_us_run <- function(matern.nu, cv) {
	print(matern.nu)
	print(cv)

    obs_full_us <- readRDS("../../data/created/obs.rds")
    cv_object <- readRDS(paste0("../../data/created/cv_objects/", cv, ".rds"))

    n_iter <- 10000
    burn <- 2000
    thin <- 4


    time_fit <- system.time({
    ctm_fit <- grmbayes::grm(Y = obs_full_us$pm_aqs,
                             X = obs_full_us$pm25_tot_ncar,
                             n.iter = n_iter,
                             burn = burn,
                             thin = thin,
                             nngp = T,
                             covariance = "matern",
                             matern.nu = matern.nu,
                             theta.alpha.a = 3,
                             theta.alpha.b = 200,
                             theta.beta.a = 3,
                             theta.beta.b = 200,
                             theta.alpha.tune = 0.05,
                             theta.beta.tune = 0.05,
                             theta.alpha.init = 100,
                             theta.beta.init = 100,
                             sigma.fix.iter.num = 250,
                             tau.alpha.a = 0.005,
                             tau.alpha.b = 0.005,
                             tau.beta.a = 0.005,
                             tau.beta.b = 0.005,
                             tau.alpha.tune = 0.05,
                             tau.beta.tune = 0.05,
                             discrete.theta.gibbs = F,
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
                             theta.alpha.a = 3,
                             theta.alpha.b = 200,
                             theta.beta.a = 3,
                             theta.beta.b = 200,
                             theta.alpha.tune = 0.05,
                             theta.beta.tune = 0.05,
                             theta.alpha.init = 100,
                             theta.beta.init = 100,
                             sigma.fix.iter.num = 250,
                             tau.alpha.a = 0.005,
                             tau.alpha.b = 0.005,
                             tau.beta.a = 0.005,
                             tau.beta.b = 0.005,
                             tau.alpha.tune = 0.05,
                             tau.beta.tune = 0.05,
                             discrete.theta.gibbs = F,
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
            paste0("../../output/results/full_us_fit_",
                   matern.nu,
                   "_", 
                   cv, 
                   ".RDS"))
}
