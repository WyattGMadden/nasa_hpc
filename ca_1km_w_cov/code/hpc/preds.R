full_preds <- function() {

    #best rmse matern.nu
    fit <- readRDS("~/../../projects/hhchang/wmadden/nasa_hpc/ca_1km_w_cov/output/results/fits/fit_0.5_ordinary.RDS")$ctm_fit

    obs <- readRDS("../../data/created/obs.rds")
    pred_dat <- readRDS("../../data/created/preds.rds")
    names(pred_dat)


    cat("start predictions \n")

    preds <- grmbayes::grm_pred(grm.fit = fit,
                                X.pred = pred_dat$aod.final,
                                L.pred = pred_dat[, c("elevation", "population")],
                                M.pred = pred_dat[, c("cloud", "v_wind", "hpbl",
                                                      "u_wind", "short_rf", "humidity_2m")],
                                coords.Y = obs[, c("x", "y")],
                                space.id.Y = obs$space_id,
                                coords.pred = pred_dat[, c("x", "y")],
                                space.id = pred_dat$space_id,
                                time.id = pred_dat$time_id,
                                spacetime.id = pred_dat$spacetime_id,
                                n.iter = 2000,
                                verbose = T,
                                in.sample = F,
                                include.random.effects = T)

    cat("save predictions \n")

    saveRDS(preds,
            "~/../../projects/hhchang/wmadden/nasa_hpc/ca_1km_w_cov/output/results/preds/preds.RDS")

    cat("job done \n")
}
