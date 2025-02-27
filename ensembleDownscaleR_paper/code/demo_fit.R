
library(ensembleDownscaleR)
set.seed(42)

# start timer
start_time <- proc.time()




# Stage 1

monitor_pm25_with_cmaq <- readRDS("../data/monitor_pm25_with_cmaq.rds")

cmaq_fit <- grm(
    Y = monitor_pm25_with_cmaq$pm25,
    X = monitor_pm25_with_cmaq$cmaq,
    L = monitor_pm25_with_cmaq[, c("elevation", "population")],
    M = monitor_pm25_with_cmaq[, c("cloud", "v_wind", "hpbl", 
                                   "u_wind", "short_rf", "humidity_2m")],
    n.iter = 2500,
    burn = 500,
    thin = 4,
    covariance = "matern",
    matern.nu = 0.5,
    coords = monitor_pm25_with_cmaq[, c("x", "y")],
    space.id = monitor_pm25_with_cmaq$space_id,
    time.id = monitor_pm25_with_cmaq$time_id,
    spacetime.id = monitor_pm25_with_cmaq$spacetime_id,
    verbose.iter = 10
)

saveRDS(cmaq_fit, "../output/fit_pred_objects/cmaq_fit.rds")
cmaq_fit <- readRDS("../output/fit_pred_objects/cmaq_fit.rds")


monitor_pm25_with_aod <- readRDS("../data/monitor_pm25_with_aod.rds")


aod_fit <- grm(
    Y = monitor_pm25_with_aod$pm25,
    X = monitor_pm25_with_aod$aod,
    L = monitor_pm25_with_aod[, c("elevation", "population")],
    M = monitor_pm25_with_aod[, c("cloud", "v_wind", "hpbl", 
                                   "u_wind", "short_rf", "humidity_2m")],
    n.iter = 2500,
    burn = 500,
    thin = 4,
    coords = monitor_pm25_with_aod[, c("x", "y")],
    space.id = monitor_pm25_with_aod$space_id,
    time.id = monitor_pm25_with_aod$time_id,
    spacetime.id = monitor_pm25_with_aod$spacetime_id,
    verbose.iter = 10
)

saveRDS(aod_fit, "../output/fit_pred_objects/aod_fit.rds")
aod_fit <- readRDS("../output/fit_pred_objects/aod_fit.rds")


# Stage 2


cv_id_cmaq_ord <- create_cv(
    time.id = monitor_pm25_with_cmaq$time_id, 
    space.id = monitor_pm25_with_cmaq$space_id,
    spacetime.id = monitor_pm25_with_cmaq$spacetime_id,
    type = "ordinary"
)

saveRDS(cv_id_cmaq_ord, "../output/fit_pred_objects/cv_id_cmaq_ord.rds")
cv_id_cmaq_ord <- readRDS("../output/fit_pred_objects/cv_id_cmaq_ord.rds")


cmaq_fit_cv <- grm_cv(
    Y = monitor_pm25_with_cmaq$pm25,
    X = monitor_pm25_with_cmaq$cmaq,
    cv.object = cv_id_cmaq_ord,
    L = monitor_pm25_with_cmaq[, c("elevation", "population")],
    M = monitor_pm25_with_cmaq[, c("cloud", "v_wind", "hpbl", 
                                   "u_wind", "short_rf", "humidity_2m")],
    n.iter = 2500,
    burn = 500,
    thin = 4,
    coords = monitor_pm25_with_cmaq[, c("x", "y")],
    space.id = monitor_pm25_with_cmaq$space_id,
    time.id = monitor_pm25_with_cmaq$time_id,
    spacetime.id = monitor_pm25_with_cmaq$spacetime_id,
    verbose.iter = 10
)

saveRDS(cmaq_fit_cv, "../output/fit_pred_objects/cmaq_fit_cv.rds")
cmaq_fit_cv <- readRDS("../output/fit_pred_objects/cmaq_fit_cv.rds")


cv_id_aod_ord <- create_cv(
    time.id = monitor_pm25_with_aod$time_id,
    space.id = monitor_pm25_with_aod$space_id,
    spacetime.id = monitor_pm25_with_aod$spacetime_id,
    type = "ordinary"
)


saveRDS(cv_id_aod_ord, "../output/fit_pred_objects/cv_id_aod_ord.rds")
cv_id_aod_ord <- readRDS("../output/fit_pred_objects/cv_id_aod_ord.rds")

aod_fit_cv <- grm_cv(
    Y = monitor_pm25_with_aod$pm25,
    X = monitor_pm25_with_aod$aod,
    cv.object = cv_id_aod_ord,
    L = monitor_pm25_with_aod[, c("elevation", "population")],
    M = monitor_pm25_with_aod[, c("cloud", "v_wind", "hpbl", 
                                   "u_wind", "short_rf", "humidity_2m")],
    n.iter = 2500,
    burn = 500,
    thin = 4,
    coords = monitor_pm25_with_aod[, c("x", "y")],
    space.id = monitor_pm25_with_aod$space_id,
    time.id = monitor_pm25_with_aod$time_id,
    spacetime.id = monitor_pm25_with_aod$spacetime_id,
    verbose.iter = 10
)


saveRDS(aod_fit_cv, "../output/fit_pred_objects/aod_fit_cv.rds")
aod_fit_cv <- readRDS("../output/fit_pred_objects/aod_fit_cv.rds")


# Stage 3

cmaq_for_predictions <- readRDS("../data/cmaq_for_predictions.rds")

cmaq_pred <- grm_pred(
    grm.fit = cmaq_fit,
    X = cmaq_for_predictions$cmaq,
    L = cmaq_for_predictions[, c("elevation", "population")],
    M = cmaq_for_predictions[, c("cloud", "v_wind", "hpbl",
                                 "u_wind", "short_rf", "humidity_2m")],
    coords = cmaq_for_predictions[, c("x", "y")],
    space.id = cmaq_for_predictions$space_id,
    time.id = cmaq_for_predictions$time_id,
    spacetime.id = cmaq_for_predictions$spacetime_id,
    n.iter = 500,
    verbose = T
)

saveRDS(cmaq_pred, "../output/fit_pred_objects/cmaq_pred.rds")
cmaq_pred <- readRDS("../output/fit_pred_objects/cmaq_pred.rds")



aod_for_predictions <- readRDS("../data/aod_for_predictions.rds")


aod_pred <- grm_pred(
    grm.fit = aod_fit,
    X = aod_for_predictions$aod,
    L = aod_for_predictions[, c("elevation", "population")],
    M = aod_for_predictions[, c("cloud", "v_wind", "hpbl", 
                                        "u_wind", "short_rf", "humidity_2m")],
    coords = aod_for_predictions[, c("x", "y")],
    space.id = aod_for_predictions$space_id,
    time.id = aod_for_predictions$time_id,
    spacetime.id = aod_for_predictions$spacetime_id,
    n.iter = 500,
    verbose = T
)

saveRDS(aod_pred, "../output/fit_pred_objects/aod_pred.rds")
aod_pred <- readRDS("../output/fit_pred_objects/aod_pred.rds")


# Stage 4

ensemble_fit <- ensemble_spatial(
    grm.fit.cv.1 = cmaq_fit_cv,
    grm.fit.cv.2 = aod_fit_cv,
    n.iter = 2500,
    burn = 500,
    thin = 4,
    tau.a = 0.001,
    tau.b = 0.001,
    theta.tune = 0.2,
    theta.a = 5,
    theta.b = 0.05
)


saveRDS(ensemble_fit, "../output/fit_pred_objects/ensemble_fit.rds")
ensemble_fit <- readRDS("../output/fit_pred_objects/ensemble_fit.rds")


# Other 

ensemble_preds_at_observations <- gap_fill(
    grm.pred.1 = cmaq_fit_cv,
    grm.pred.2 = aod_fit_cv,
    weights = ensemble_fit
)

saveRDS(ensemble_preds_at_observations, "../output/fit_pred_objects/ensemble_preds_at_observations.rds")
ensemble_preds_at_observations <- readRDS("../output/fit_pred_objects/ensemble_preds_at_observations.rds")

# Stage 5

weight_preds <- weight_pred(
    ensemble.fit = ensemble_fit,
    coords = cmaq_for_predictions[, c("x", "y")],
    space.id = cmaq_for_predictions$space_id,
    verbose = T
)


saveRDS(weight_preds, "../output/fit_pred_objects/weight_preds.rds")
weight_preds <- readRDS("../output/fit_pred_objects/weight_preds.rds")

# Stage 6

results <- gap_fill(
    grm.pred.1 = cmaq_pred,
    grm.pred.2 = aod_pred,
    weights = weight_preds)

saveRDS(results, "../output/fit_pred_objects/results.rds")
results <- readRDS("../output/fit_pred_objects/results.rds")

# stop timer
end_time <- proc.time()
total_runtime <- end_time - start_time
saveRDS(total_runtime, "../output/fit_pred_objects/runtime.rds")


