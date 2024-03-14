


dirs <- list.files("../../data/created/prelim_fit",
                   full.names = T)

all <- lapply(dirs,
              function(x) {
                  temp <- readRDS(x)
                  temp$matern_nu <- substr(x, 
                                           nchar(x) - 6, 
                                           nchar(x) - 4) |>
                    as.numeric()
                  return(temp)
              })


others_out <- lapply(all,
                 function(x) {
                     fit <- x$others
                     fit$matern_nu <- x$matern_nu
                     fit$iter <- 1:nrow(fit)
                     return(fit)
                    }
                 ) |>
    (\(.) Reduce(rbind, .))()

temp <- all[[2]]
temp$theta.acc
temp$theta.acc

dinvgamma <- function(x, alpha, beta) dgamma(1/x, alpha, beta) * (1/x)^2
dgamma(1.5, 0.5, 0.5)

x <- seq(0.01, 200, 0.01)
y <- dgamma(x, 0.005, 0.005)
y <- dinvgamma(x, 3, 200)
plot(x, y)

library(tidyverse)
sums <- others_out |>
    group_by(matern_nu) |>
    filter(iter > 200,
           iter %% 4 == 0) |>
    summarize(theta_alpha_mean = mean(theta.alpha),
              theta_beta_mean = mean(theta.beta),
              theta_alpha_sd = sd(theta.alpha),
              theta_beta_sd = sd(theta.beta),
              theta_alpha_max = max(theta.alpha),
              theta_beta_max = max(theta.beta),
              theta_alpha_min = min(theta.alpha),
              theta_beta_min = min(theta.beta))
              


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


matern_kernal <- function(distance, theta, nu) {

    if (!(nu %in% c(0.5, 1.5, 2.5))) {
        stop("nu must be one of 0.5, 1.5, or 2.5")
    }
    
    root_2_nu_distance_theta <- sqrt(2 * nu) * distance / theta

    if (nu == 0.5) {

        kern <- exp(-root_2_nu_distance_theta)

    } else if (nu == 1.5) {

        kern <- (1 + root_2_nu_distance_theta) * exp(-root_2_nu_distance_theta)

    } else if (nu == 2.5) {
        
        kern <- (1 + root_2_nu_distance_theta + root_2_nu_distance_theta^2 / 3) * exp(-root_2_nu_distance_theta)

    }

    return(kern)
}



get_buffer_cor <- function(nu, corr, others_out) {
    others_out_tb <- others_out[others_out$iter > 200 & (others_out$iter %% 4 == 0),]

    mean_theta_alpha <- mean(others_out_tb$theta.alpha[others_out_tb$matern_nu == nu])
    mean_theta_beta <- mean(others_out_tb$theta.beta[others_out_tb$matern_nu == nu])
    mean_theta <- max(c(mean_theta_alpha, mean_theta_beta))

    #range of reasonable distances
    seq_dist <- seq(0.1, 1000, 0.1)

    #get distance for desired correlation
    buffer <- seq_dist[which.min(abs(matern_kernal(seq_dist, mean_theta, nu) - corr))]

    return(buffer)
}
buffer_params <- expand.grid(nu = seq(0.5, 2.5, 1), 
                           corr = c(0.7, 0.3)) |>
    group_by(nu, corr) |>
    summarize(distance = get_buffer_cor(nu, corr, others_out))

write.csv(buffer_params, "../../data/created/buffer_params.csv")

