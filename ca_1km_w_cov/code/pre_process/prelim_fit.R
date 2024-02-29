fit_mat <- function(matern.nu) {
    obs <- readRDS("../../data/created/obs.rds")
    matern.nu <- as.integer(matern.nu) - 0.5
    cat("###################")
    cat("###################")
    cat(paste0("matern nu = ", matern.nu))
    cat("###################")
    cat("###################")
    cat("")
    cat("")
    cat("")
    cat("")


    temp_fit <- grmbayes::grm(Y = obs$pm25,
                             X = obs$aod.final,
                             n.iter = 1000,
                             burn = 200,
                             thin = 1,
                             nngp = T,
                             L = obs[, c("elevation", "population")],
                             M = obs[, c("cloud", "v_wind", "hpbl",
                                         "u_wind", "short_rf", "humidity_2m")],
                             covariance = "matern",
                             matern.nu = matern.nu,
                             theta.alpha.a = 0.005,
                              theta.alpha.b = 0.005,
                              theta.beta.a = 0.005,
                              theta.beta.b = 0.005,
                              theta.alpha.init = 20,
                              theta.beta.init = 20,
                              tau.alpha.a = 0.005,
                              tau.alpha.b = 0.005,
                              tau.beta.a = 0.005,
                              tau.beta.b = 0.005,
                              tau.alpha.tune = 0.05,
                              tau.beta.tune = 0.05,
                              discrete.theta.gibbs = F,
                             coords = obs[, c("x", "y")],
                             space.id = obs$space_id,
                             time.id = obs$time_id,
                             spacetime.id = obs$spacetime_id,
                             verbose.iter = 10)
    cat("")
    cat("")
    cat("")
    cat("")
    saveRDS(temp_fit, 
            paste0("../../data/created/prelim_fit/prelim_fit_", matern.nu, ".rds"))
}

