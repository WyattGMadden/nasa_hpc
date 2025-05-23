fit_mat <- function(matern.nu) {
    obs <- readRDS("~/../../projects/hhchang/wmadden/nasa_hpc/ca_1km_w_cov/data/created/obs.rds")
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
                             thin = 1,
                             nngp = T,
                             burn = 0,
                             L = obs[, c("elevation", "population")],
                             M = obs[, c("cloud", "v_wind", "hpbl",
                                         "u_wind", "short_rf", "humidity_2m")],
                             covariance = "matern",
                             matern.nu = matern.nu,
                             theta.alpha.a = 0.005,
                             theta.alpha.b = 0.005,
                             theta.beta.a = 0.005,
                             theta.beta.b = 0.005,
                              #theta.alpha.init = 50,
                              #theta.beta.init = 50,
                              #theta.alpha.tune = 0.2,
                              #theta.beta.tune = 0.2,
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
            paste0("~/../../projects/hhchang/wmadden/nasa_hpc/ca_1km_w_cov/data/created/prelim_fit/prelim_fit_", matern.nu, ".rds"))
}

