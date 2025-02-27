library(ensembleDownscaleR)
set.seed(42)


# some results were previously fit in demo_fit.R, and can be recycled here


# Stage 1
monitor_pm25_with_cmaq <- readRDS("../data/monitor_pm25_with_cmaq.rds")
monitor_pm25_with_aod <- readRDS("../data/monitor_pm25_with_aod.rds")

cmaq_fit <- readRDS("../output/fit_pred_objects/cmaq_fit.rds")
aod_fit <- readRDS("../output/fit_pred_objects/aod_fit.rds")


# Determine distances at which 0.7 and 0.3 spatial effect correlation
# for spatial buffers

# Stage 2

others_cmaq <- cmaq_fit$others
others_aod <- aod_fit$others

mean_theta_alpha_cmaq <- mean(others_cmaq$theta.alpha)
mean_theta_beta_cmaq <- mean(others_cmaq$theta.beta)
mean_theta_alpha_aod <- mean(others_aod$theta.alpha)
mean_theta_beta_aod <- mean(others_aod$theta.beta)

mean_cmaq <- min(c(mean_theta_alpha_cmaq, mean_theta_beta_cmaq))
mean_aod <- min(c(mean_theta_alpha_aod, mean_theta_beta_aod))
mean_cmaq_aod <- min(c(mean_cmaq, mean_aod))



buffer_7 <- - mean_cmaq_aod * log(0.7)
buffer_3 <- - mean_cmaq_aod * log(0.3)


cv_id_cmaq_spat <- create_cv(
    time.id = monitor_pm25_with_cmaq$time_id,
    space.id = monitor_pm25_with_cmaq$space_id,
    spacetime.id = monitor_pm25_with_cmaq$spacetime_id,
    type = "spatial",
)

cv_id_aod_spat <- create_cv(
    create.from = cv_id_cmaq_spat,
    time.id = monitor_pm25_with_aod$time_id, 
    space.id = monitor_pm25_with_aod$space_id,
    spacetime.id = monitor_pm25_with_aod$spacetime_id
)

saveRDS(cv_id_cmaq_spat, "../output/additional_cv_fit_objects/cv_id_cmaq_spat.rds")
saveRDS(cv_id_aod_spat, "../output/additional_cv_fit_objects/cv_id_aod_spat.rds")



cv_id_cmaq_spatclust <- create_cv(
    time.id = monitor_pm25_with_cmaq$time_id, 
    space.id = monitor_pm25_with_cmaq$space_id,
    spacetime.id = monitor_pm25_with_cmaq$spacetime_id,
    type = "spatial_clustered",
    coords = monitor_pm25_with_cmaq[, c("x", "y")],
)

cv_id_aod_spatclust <- create_cv(
    create.from = cv_id_cmaq_spatclust,
    time.id = monitor_pm25_with_aod$time_id, 
    space.id = monitor_pm25_with_aod$space_id,
    spacetime.id = monitor_pm25_with_aod$spacetime_id,
    coords = monitor_pm25_with_aod[, c("x", "y")]
)


saveRDS(cv_id_cmaq_spatclust, "../output/additional_cv_fit_objects/cv_id_cmaq_spatclust.rds")
saveRDS(cv_id_aod_spatclust, "../output/additional_cv_fit_objects/cv_id_aod_spatclust.rds")


cv_id_cmaq_spatbuff3 <- create_cv(
    time.id = monitor_pm25_with_cmaq$time_id, 
    space.id = monitor_pm25_with_cmaq$space_id,
    spacetime.id = monitor_pm25_with_cmaq$spacetime_id,
    type = "spatial_buffered",
    coords = monitor_pm25_with_cmaq[, c("x", "y")],
    buffer.size = buffer_3
)

cv_id_aod_spatbuff3 <- create_cv(
    create.from = cv_id_cmaq_spatbuff3,
    time.id = monitor_pm25_with_aod$time_id, 
    space.id = monitor_pm25_with_aod$space_id,
    spacetime.id = monitor_pm25_with_aod$spacetime_id,
    coords = monitor_pm25_with_aod[, c("x", "y")]
)
saveRDS(cv_id_cmaq_spatbuff3, "../output/additional_cv_fit_objects/cv_id_cmaq_spatbuff3.rds")
saveRDS(cv_id_aod_spatbuff3, "../output/additional_cv_fit_objects/cv_id_aod_spatbuff3.rds")


cv_id_cmaq_spatbuff7 <- create_cv(
    space.id = monitor_pm25_with_cmaq$space_id,
    time.id = monitor_pm25_with_cmaq$time_id, 
    type = "spatial_buffered",
    coords = monitor_pm25_with_cmaq[, c("x", "y")],
    buffer.size = buffer_7
)

cv_id_aod_spatbuff7 <- create_cv(
    create.from = cv_id_cmaq_spatbuff7,
    space.id = monitor_pm25_with_aod$space_id,
    time.id = monitor_pm25_with_aod$time_id, 
    coords = monitor_pm25_with_aod[, c("x", "y")]
)

saveRDS(cv_id_cmaq_spatbuff7, "../output/additional_cv_fit_objects/cv_id_cmaq_spatbuff7.rds")
saveRDS(cv_id_aod_spatbuff7, "../output/additional_cv_fit_objects/cv_id_aod_spatbuff7.rds")


# Stage 3

cmaq_for_predictions <- readRDS("../data/cmaq_for_predictions.rds")
cmaq_pred <- readRDS("../output/fit_pred_objects/cmaq_pred.rds")

aod_for_predictions <- readRDS("../data/aod_for_predictions.rds")
aod_pred <- readRDS("../output/fit_pred_objects/aod_pred.rds")





# function that combines stage 2, 4, and 5 (using previously saved stage 1 and 3 output)

additional_cv_formulations <- function(
    monitor_pm25_with_cmaq,
    monitor_pm25_with_aod,
    cmaq_fit,
    aod_fit,
    cmaq_pred,
    aod_pred,
    cv_id_cmaq,
    cv_id_aod,
    save_dir,
    n.iter = 2500,
    burn = 500,
    thin = 4
) {
    cmaq_fit_cv <- grm_cv(
        Y = monitor_pm25_with_cmaq$pm25,
        X = monitor_pm25_with_cmaq$cmaq,
        cv.object = cv_id_cmaq,
        L = monitor_pm25_with_cmaq[, c("elevation", "population")],
        M = monitor_pm25_with_cmaq[, c("cloud", "v_wind", "hpbl", 
                                       "u_wind", "short_rf", "humidity_2m")],
        n.iter = n.iter,
        burn = burn,
        thin = thin,
        coords = monitor_pm25_with_cmaq[, c("x", "y")],
        space.id = monitor_pm25_with_cmaq$space_id,
        time.id = monitor_pm25_with_cmaq$time_id,
        spacetime.id = monitor_pm25_with_cmaq$spacetime_id,
        verbose.iter = 10
    )

    aod_fit_cv <- grm_cv(
        Y = monitor_pm25_with_aod$pm25,
        X = monitor_pm25_with_aod$aod,
        cv.object = cv_id_aod,
        L = monitor_pm25_with_aod[, c("elevation", "population")],
        M = monitor_pm25_with_aod[, c("cloud", "v_wind", "hpbl", 
                                       "u_wind", "short_rf", "humidity_2m")],
        n.iter = n.iter,
        burn = burn,
        thin = thin,
        coords = monitor_pm25_with_aod[, c("x", "y")],
        space.id = monitor_pm25_with_aod$space_id,
        time.id = monitor_pm25_with_aod$time_id,
        spacetime.id = monitor_pm25_with_aod$spacetime_id,
        verbose.iter = 10
    )

    ensemble_fit <- ensemble_spatial(
        grm.fit.cv.1 = cmaq_fit_cv,
        grm.fit.cv.2 = aod_fit_cv,
        n.iter = n.iter,
        burn = burn,
        thin = thin,
        tau.a = 0.001,
        tau.b = 0.001,
        theta.tune = 0.2,
        theta.a = 5,
        theta.b = 0.05
    )

    ensemble_preds_at_observations <- gap_fill(
        grm.pred.1 = cmaq_fit_cv,
        grm.pred.2 = aod_fit_cv,
        weights = ensemble_fit
    )

    cv_name_temp <- deparse(substitute(cv_id_aod))
    saveRDS(
        cmaq_fit_cv, 
        paste0(
            save_dir, 
            "cmaq_fit_cv_", 
            substr(cv_name_temp, 11, nchar(cv_name_temp)), 
            ".rds"
        )
    )
    saveRDS(
        aod_fit_cv, 
        paste0(
            save_dir, 
            "aod_fit_cv_", 
            substr(cv_name_temp, 11, nchar(cv_name_temp)), 
            ".rds"
        )
    )
    saveRDS(
        ensemble_preds_at_observations, 
        paste0(
            save_dir, 
            "ensemble_preds_at_observations_", 
            substr(cv_name_temp, 11, nchar(cv_name_temp)), 
            ".rds"
        )
    )
}
n.iter <- 2500
burn <- 500
thin <- 4

additional_cv_formulations(
    monitor_pm25_with_cmaq = monitor_pm25_with_cmaq,
    monitor_pm25_with_aod = monitor_pm25_with_aod,
    cmaq_fit = cmaq_fit,
    aod_fit = aod_fit,
    cmaq_pred = cmaq_pred,
    aod_pred = aod_pred,
    cv_id_cmaq = cv_id_cmaq_spat,
    cv_id_aod = cv_id_aod_spat,
    save_dir = "../output/additional_cv_fit_objects/",
    n.iter = n.iter,
    burn = burn,
    thin = thin
)

additional_cv_formulations(
    monitor_pm25_with_cmaq = monitor_pm25_with_cmaq,
    monitor_pm25_with_aod = monitor_pm25_with_aod,
    cmaq_fit = cmaq_fit,
    aod_fit = aod_fit,
    cmaq_pred = cmaq_pred,
    aod_pred = aod_pred,
    cv_id_cmaq = cv_id_cmaq_spatclust,
    cv_id_aod = cv_id_aod_spatclust,
    save_dir = "../output/additional_cv_fit_objects/",
    n.iter = n.iter,
    burn = burn,
    thin = thin
)


additional_cv_formulations(
    monitor_pm25_with_cmaq = monitor_pm25_with_cmaq,
    monitor_pm25_with_aod = monitor_pm25_with_aod,
    cmaq_fit = cmaq_fit,
    aod_fit = aod_fit,
    cmaq_pred = cmaq_pred,
    aod_pred = aod_pred,
    cv_id_cm = cv_id_cmaq_spatbuff3,
    cv_id_aod = cv_id_aod_spatbuff3,
    save_dir = "../output/additional_cv_fit_objects/",
    n.iter = n.iter,
    burn = burn,
    thin = thin
)

additional_cv_formulations(
    monitor_pm25_with_cmaq = monitor_pm25_with_cmaq,
    monitor_pm25_with_aod = monitor_pm25_with_aod,
    cmaq_fit = cmaq_fit,
    aod_fit = aod_fit,
    cmaq_pred = cmaq_pred,
    aod_pred = aod_pred,
    cv_id_cmaq = cv_id_cmaq_spatbuff7,
    cv_id_aod = cv_id_aod_spatbuff7,
    save_dir = "../output/additional_cv_fit_objects/",
    n.iter = n.iter,
    burn = burn,
    thin = thin
)




