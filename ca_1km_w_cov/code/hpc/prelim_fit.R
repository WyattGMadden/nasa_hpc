
obs <- readRDS("../../data/created/obs.rds")

discrete.theta.alpha.values <- seq(50, 250, 25)       
discrete.theta.beta.values <- seq(50, 250, 25)       
n_iter <- 100
burn <- 20
thin <- 4
names(obs)
ctm_fit <- grmbayes::grm(Y = obs$pm25,
                         X = obs$aod.final,
                         n.iter = n_iter,
                         burn = burn,
                         thin = thin,
                         nngp = T,
                         L = obs[, c("elevation", "population")],
                         M = obs[, c("cloud", "v_wind", "hpbl",
                                     "u_wind", "short_rf", "humidity_2m")],
                         discrete.theta.alpha.values = discrete.theta.alpha.values,
                         discrete.theta.beta.values = discrete.theta.beta.values,
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

cv_object <- readRDS("../../data/created/cv_objects/ordinary.rds")


ctm_fit_cv <- grmbayes::grm_cv(Y = obs$pm25,
                         X = obs$aod.final,
                         cv.object = cv_object,
                         n.iter = n_iter,
                         burn = burn,
                         thin = thin,
                         nngp = T,
                         covariance = "exponential",
                         L = obs[, c("elevation", "population")],
                         M = obs[, c("cloud", "v_wind", "hpbl",
                                     "u_wind", "short_rf", "humidity_2m")],
                         discrete.theta.gibbs = F,
                         discrete.theta.alpha.values = discrete.theta.alpha.values,
                         discrete.theta.beta.values = discrete.theta.beta.values,
                         coords = obs[, c("x", "y")],
                         space.id = obs$space_id,
                         time.id = obs$time_id,
                         spacetime.id = obs$spacetime_id,
                         verbose.iter = 10)


