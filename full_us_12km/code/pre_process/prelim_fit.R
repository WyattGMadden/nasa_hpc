
obs_full_us <- readRDS("../../data/created/obs.rds")

matern.nu <- c(0.5, 1.5, 2.5)


fit_mat <- function(matern.nu, obs_full_us) {
    cat("###################")
    cat("###################")
    cat(paste0("matern nu = ", matern.nu))
    cat("###################")
    cat("###################")
    cat("")
    cat("")
    cat("")
    cat("")
    if (matern.nu == 0.5) {
        theta_ab <- 0.005
        theta_init <- 0.5
    } else {
        theta_ab <- 0.05
        theta_init <- 0.5
    }
    temp_fit <- grmbayes::grm(Y = obs_full_us$pm_aqs,
                              X = obs_full_us$pm25_tot_ncar,
                              n.iter = 1000,
                              burn = 200,
                              thin = 1,
                              nngp = T,
                              covariance = "matern",
                              matern.nu = matern.nu,
                              theta.alpha.a = theta_ab,
                              theta.alpha.b = theta_ab,
                              theta.beta.a = theta_ab,
                              theta.beta.b = theta_ab,
                              theta.alpha.init = theta_init,
                              theta.beta.init = theta_init,
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
    return(temp_fit)
}

fit_mat <- lapply(matern.nu, 
                  function(x) fit_mat(matern.nu = x, 
                                      obs_full_us = obs_full_us))

for (i in 1:length(fit_mat)) {
    fit_mat[[i]]$matern.nu <- matern.nu[i]
}

saveRDS(fit_mat, paste0("../../data/created/prelim_fits.rds"))
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

x <- seq(0, 100, 0.001)
y <- dgamma(x, .005, 0.005)
plot(x, y, type = "l")
library(ggplot2)
others_out |>
    ggplot(aes(x = iter, y = theta.alpha)) + 
    geom_line() +
    facet_grid(matern_nu ~ ., scales = "free")
others_out |>
    ggplot(aes(x = iter, y = theta.beta)) + 
    geom_line() +
    facet_grid(matern_nu ~ ., scales = "free")

others_out |>
    ggplot(aes(x = iter, y = tau.alpha)) + 
    geom_line() +
    facet_grid(matern_nu ~ ., scales = "free")
others_out |>
    ggplot(aes(x = iter, y = tau.beta)) + 
    geom_line() +
    facet_grid(matern_nu ~ ., scales = "free")

others_out |>
    ggplot(aes(x = iter, y = sigma2)) + 
    geom_line() +
    facet_grid(matern_nu ~ ., scales = "free")
others_out |>
    ggplot(aes(x = iter, y = alpha0)) + 
    geom_line() +
    facet_grid(matern_nu ~ ., scales = "free")
others_out |>
    ggplot(aes(x = iter, y = beta0)) + 
    geom_line() +
    facet_grid(matern_nu ~ ., scales = "free")


###output correlation###
#based on max(theta_alpha, theta_beta), nu = 0.5
#correlation of 0.7 and 0.3

mean_theta_alpha <- mean(others_out$theta.alpha[others_out$matern_nu == 0.5])
mean_theta_beta <- mean(others_out$theta.beta[others_out$matern_nu == 0.5])
mean_theta <- max(c(mean_theta_alpha, mean_theta_beta))


#inverse correlation
distance <- - mean_theta * log(correlation)
buffer_0.8 <- - mean_theta * log(0.7)
buffer_0.5 <- - mean_theta * log(0.3)





