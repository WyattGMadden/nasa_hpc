
output <- readRDS("../../output/results/fit_0.5_ordinary.RDS")
ctm_fit <- output$ctm_fit

obs <- readRDS("../../data/created/obs.rds")



L.pred = as.matrix(obs[, c("elevation", "population")])
M.pred = as.matrix(obs[, c("cloud", "v_wind", "hpbl", "u_wind", "short_rf", "humidity_2m")])

?grmbayes::grm_pred
ctm_preds <- grmbayes::grm_pred(grm.fit = ctm_fit,
                                X.pred = obs$aod.final,
                                L.pred = L.pred,
                                M.pred = M.pred,
                                coords.Y = obs[, c("x", "y")],
                                space.id.Y = obs$space_id,
                                coords.pred = obs[, c("x", "y")],
                                space.id = obs$space_id,
                                time.id = obs$time_id,
                                spacetime.id = obs$spacetime_id,
                                n.iter = 2000,
                                verbose = T,
                                in.sample = TRUE)
?saveRDS
saveRDS(ctm_preds, "../../output/results/within_sample_preds/preds_0.5_ordinary_within_sample.RDS") 
