
fit_mat <- lapply(matern.nu, 
                  function(x) fit_mat(matern.nu = x, 
                                      obs_full_us = obs_full_us))

for (i in 1:length(fit_mat)) {
    fit_mat[[i]]$matern.nu <- matern.nu[i]
}

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



#summary(others_out$theta.alpha)
#summary(others_out$theta.beta)
