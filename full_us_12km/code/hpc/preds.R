us_preds <- function() {

    #best rmse matern.nu
    fit <- readRDS("../../output/results/fits/full_us_fit_0.5_ordinary.RDS")$ctm_fit

    obs <- readRDS("../../data/created/obs.rds")
    pred_dat <- readRDS("../../data/created/preds.rds")

    cat("start predictions \n")

    preds <- grmbayes::grm_pred(grm.fit = fit,
                                X.pred = pred_dat$pm25_tot_ncar,
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
            "../../output/results/preds/preds.RDS")

    cat("job done \n")

}
