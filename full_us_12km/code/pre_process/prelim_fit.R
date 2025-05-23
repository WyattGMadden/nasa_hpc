fit_mat <- function(matern.nu) {
    obs_full_us <- readRDS("~/../../projects/hhchang/wmadden/nasa_hpc/full_us_12km/data/created/obs.rds")
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


    temp_fit <- grmbayes::grm(Y = obs_full_us$pm_aqs,
                              X = obs_full_us$pm25_tot_ncar,
                              n.iter = 5000,
                              burn = 0,
                              thin = 1,
                              nngp = T,
                              covariance = "matern",
                              matern.nu = matern.nu,
                              theta.alpha.a = 3,
                              theta.alpha.b = 200,
                              theta.beta.a = 3,
                              theta.beta.b = 200,
                              theta.alpha.tune = 0.025,
                              theta.beta.tune = 0.025,
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
                              verbose.iter = 1)
    cat("")
    cat("")
    cat("")
    cat("")
    saveRDS(temp_fit, 
            paste0("~/../../projects/hhchang/wmadden/nasa_hpc/full_us_12km/data/created/prelim_fit/prelim_fit_", matern.nu, ".rds"))
}


