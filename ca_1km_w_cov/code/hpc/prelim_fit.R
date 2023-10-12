
obs <- readRDS("../../data/created/obs.rds")
ctm_fit <- grmbayes::grm(Y = obs$pm25,
                         X = obs$aod.final,
                         n.iter = 1000,
                         burn = 200,
                         thin = 4,
                         nngp = T,
                         covariance = "exponential",
                         coords = obs[, c("x", "y")],
                         space.id = obs$space_id,
                         time.id = obs$time_id,
                         spacetime.id = obs$spacetime_id,
                         verbose.iter = 10)

plot(ctm_fit$others$theta.alpha)
plot(ctm_fit$others$theta.beta)
plot(ctm_fit$others$tau.alpha)
plot(ctm_fit$others$tau.beta)

summary(ctm_fit$others$theta.alpha)
summary(ctm_fit$others$theta.beta)
ctm_fit$others$theta.beta

exp(-0 / 100) / exp(-100/100)


