


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

temp <- all[[1]]
temp$theta.acc
temp$theta.acc

dinvgamma <- function(x, alpha, beta) dgamma(1/x, alpha, beta) * (1/x)^2
dgamma(1.5, 0.5, 0.5)

x <- seq(0.01, 200, 0.01)
y <- dgamma(x, 0.005, 0.005)
y <- dinvgamma(x, 3, 200)
plot(x, y)

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
    ggplot(aes(x = iter, y = rho.alpha)) + 
    geom_line() +
    facet_grid(matern_nu ~ ., scales = "free")

others_out |>
    ggplot(aes(x = iter, y = rho.beta)) + 
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
