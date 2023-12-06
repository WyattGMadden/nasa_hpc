full_preds <- function() {

    #best rmse matern.nu
    fit <- readRDS("../../output/results/fits/fit_0.5_ordinary.RDS")$ctm_fit

    pred_dat <- readRDS("../../data/created/preds.rds")



    preds <- grmbayes::grm_pred(grm.fit = fit,
                                X.pred = pred_dat$aod.final,
                                L.pred = pred_dat[, c("elevation", "population")],
                                M.pred = pred_dat[, c("cloud", "v_wind", "hpbl", 
                                                 "u_wind", "short_rf", 
                                                 "humidity_2m")],
                                coords.Y = obs[, c("x", "y")],
                                space.id.Y = obs$space_id,
                                coords.pred = pred_dat[, c("x", "y")],
                                space.id = pred_dat$space_id,
                                time.id = pred_dat$time_id,
                                spacetime.id = pred_dat$spacetime_id,
                                n.iter = 10,
                                verbose = T,
                                in.sample = TRUE)


    saveRDS(preds,
            "../../output/results/preds/preds.RDS")
}