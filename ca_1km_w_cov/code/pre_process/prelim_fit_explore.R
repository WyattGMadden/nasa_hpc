


dirs <- list.files("../../data/created/prelim_fit/",
                   full.names = T)
fit_mat <- readRDS("../../data/created/prelim_fits.rds")

all <- lapply(dirs,
              readRDS)



others_out <- lapply(all,
                 function(x) {
                     fit <- x$others
                     fit$matern_nu <- x$matern.nu
                     fit$iter <- 1:nrow(fit)
                     return(fit)
                    }
                 ) |>
    (\(.) Reduce(rbind, .))()



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
