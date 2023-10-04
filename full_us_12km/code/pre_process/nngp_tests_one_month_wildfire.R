#devtools::install_github("wyattgmadden/ensembleDownscaleR")
library(ensembleDownscaleR)
dat = read.csv("../../data/Census_PM.csv")
dat = read.csv("../../cdc_wildfire/data/Census_PM.csv")
dat = subset(dat, quarter == "2018Q4" )
names(dat) <- tolower(names(dat))
file.out = "CV_2018Q4"

dat$date = as.Date(dat$date)
dat$space_id = as.numeric(as.factor(dat$aqs_site_id))
dat$time_id = as.numeric(as.factor(as.numeric(dat$date)))
dat$spacetime_id = rep(1, nrow(dat))
dat$x <- dat$x_aqs_km
dat$y <- dat$y_aqs_km
dat <- dat[, !(names(dat) %in% c("x_aqs_km", "y_aqs_km"))]

dat <- dat[substr(dat$date, 6, 7) == "10", ]

#re-assign space_id so it's complete and sequential
dat$space_id <- as.numeric(as.factor(dat$space_id))


################
###NNGP tests###
#################

cat("#########################\n \n \n \n \n \n         TEST 1 \n \n \n \n \n \n#########################")

time_nngp <- system.time({
ctm_fit_nngp <- grm(Y = dat$pm_aqs,
               X = dat$daily_mean_pm25_tot_ncar,
               L = model.matrix(~factor(dat$location))[,-1],
               M = model.matrix(~factor(dat$days))[,-1], 
               n.iter = 400, burn = 100, thin = 4, 
               coords = dat[, c("x", "y")],
               nngp = T,
               num_neighbors = 925,
               space.id = dat$space_id,
               time.id = dat$time_id,
               spacetime.id = dat$spacetime_id,
               verbose = T,
               verbose.iter = 1
)
})


summary(ctm_fit_nngp$others$theta.beta)
summary(ctm_fit_nngp$others$theta.alpha)
#saveRDS(ctm_fit_nngp, "../output/nngp_test_one_month_wildfire/ctm_fit_nngp.rds")
#ctm_fit_nngp <- readRDS("../output/nngp_test_one_month_wildfire/ctm_fit_nngp.rds")


cat("#########################\n \n \n \n \n \n         TEST 2 \n \n \n \n \n \n#########################")
cv_id_ctm_ord <- create_cv(space.id = dat$space_id,
                           time.id = dat$time_id,
                           type = "ordinary")

time_nngp_cv <- system.time({
ctm_fit_cv_nngp <- grm_cv(Y = dat$pm_aqs,
                     cv.object = cv_id_ctm_ord,
                     X = dat$daily_mean_pm25_tot_ncar,
                     L = model.matrix(~factor(dat$location))[,-1],
                     M = model.matrix(~factor(dat$days))[,-1], 
                     n.iter = 5000, burn = 1000, thin = 4, 
                     coords = dat[, c("x", "y")],
                     nngp = T,
                     num_neighbors = 10,
                     space.id = dat$space_id,
                     time.id = dat$time_id,
                     spacetime.id = dat$spacetime_id,
                     verbose = T,
                     verbose.iter = 1)

})

saveRDS(ctm_fit_cv_nngp, "../output/nngp_test_one_month_wildfire/ctm_fit_cv_nngp.rds")
#ctm_fit_cv_nngp <- readRDS("../output/nngp_test_one_month_wildfire/ctm_fit_cv_nngp.rds")


cat("#########################\n \n \n \n \n \n         TEST 3 \n \n \n \n \n \n#########################")

#############################
###No NNGP tests (regular)###
#############################

time_reg <- system.time({
ctm_fit <- grm(Y = dat$pm_aqs,
               X = dat$daily_mean_pm25_tot_ncar,
               L = model.matrix(~factor(dat$location))[,-1],
               M = model.matrix(~factor(dat$days))[,-1], 
               n.iter = 5000, burn = 1000, thin = 4, 
               coords = dat[, c("x", "y")],
               nngp = F,
               space.id = dat$space_id,
               time.id = dat$time_id,
               spacetime.id = dat$spacetime_id,
               verbose = T,
               verbose.iter = 1
)
})



saveRDS(ctm_fit, "../output/nngp_test_one_month_wildfire/ctm_fit.rds")
#ctm_fit <- readRDS("../output/nngp_test_one_month_wildfire/ctm_fit.rds")



cat("#########################\n \n \n \n \n \n         TEST 4 \n \n \n \n \n \n#########################")


cv_id_ctm_ord <- create_cv(space.id = dat$space_id,
                           time.id = dat$time_id,
                           type = "ordinary")

time_reg_cv <- system.time({
ctm_fit_cv <- grm_cv(Y = dat$pm_aqs,
                     cv.object = cv_id_ctm_ord,
                     X = dat$daily_mean_pm25_tot_ncar,
                     L = model.matrix(~factor(dat$location))[,-1],
                     M = model.matrix(~factor(dat$days))[,-1], 
                     n.iter = 5000, burn = 1000, thin = 4, 
                     coords = dat[, c("x", "y")],
                     nngp = F,
                     space.id = dat$space_id,
                     time.id = dat$time_id,
                     spacetime.id = dat$spacetime_id,
                     verbose = T,
                     verbose.iter = 1)
})

saveRDS(ctm_fit_cv, "../output/nngp_test_one_month_wildfire/ctm_fit_cv.rds")
#ctm_fit_cv <- readRDS("../output/nngp_test_one_month_wildfire/ctm_fit_cv.rds")



cat("#########################\n \n \n \n \n \n         TEST 5 \n \n \n \n \n \n#########################")

#################################
###No NNGP tests (discrete MH)###
#################################

time_mh <- system.time({
ctm_fit_mh <- grm(Y = dat$pm_aqs,
               X = dat$daily_mean_pm25_tot_ncar,
               L = model.matrix(~factor(dat$location))[,-1],
               M = model.matrix(~factor(dat$days))[,-1], 
               n.iter = 5000, burn = 1000, thin = 4, 
               discrete.theta.alpha.values = seq(5, 150, 5),
               discrete.theta.beta.values = seq(5, 150, 5),
               discrete.theta.gibbs = F,
               coords = dat[, c("x", "y")],
               nngp = F,
               space.id = dat$space_id,
               time.id = dat$time_id,
               spacetime.id = dat$spacetime_id,
               verbose = T,
               verbose.iter = 1
)
})

saveRDS(ctm_fit_mh, "../output/nngp_test_one_month_wildfire/ctm_fit_disc_mh.rds")
#ctm_fit_mh <- readRDS("../output/nngp_test_one_month_wildfire/ctm_fit_disc_mh.rds")



cat("#########################\n \n \n \n \n \n         TEST 6 \n \n \n \n \n \n#########################")


cv_id_ctm_ord_mh <- create_cv(space.id = dat$space_id,
                           time.id = dat$time_id,
                           type = "ordinary")
time_mh_cv <- system.time({
ctm_fit_cv_mh <- grm_cv(Y = dat$pm_aqs,
                     cv.object = cv_id_ctm_ord_mh,
                     X = dat$daily_mean_pm25_tot_ncar,
                     L = model.matrix(~factor(dat$location))[,-1],
                     M = model.matrix(~factor(dat$days))[,-1], 
                     n.iter = 5000, burn = 1000, thin = 4, 
                     discrete.theta.alpha.values = seq(5, 150, 5),
                     discrete.theta.beta.values = seq(5, 150, 5),
                     discrete.theta.gibbs = F,
                     coords = dat[, c("x", "y")],
                     nngp = F,
                     space.id = dat$space_id,
                     time.id = dat$time_id,
                     spacetime.id = dat$spacetime_id,
                     verbose = T,
                     verbose.iter = 1)
})

saveRDS(ctm_fit_cv_mh, "../output/nngp_test_one_month_wildfire/ctm_fit_cv_disc_mh.rds")
#ctm_fit_cv_mh <- readRDS("../output/nngp_test_one_month_wildfire/ctm_fit_cv_disc_mh.rds")




cat("#########################\n \n \n \n \n \n         TEST 7 \n \n \n \n \n \n#########################")


####################################
###No NNGP tests (discrete Gibbs)###
####################################

time_gibbs <- system.time({
ctm_fit_gibbs <- grm(Y = dat$pm_aqs,
               X = dat$daily_mean_pm25_tot_ncar,
               L = model.matrix(~factor(dat$location))[,-1],
               M = model.matrix(~factor(dat$days))[,-1], 
               n.iter = 5000, burn = 1000, thin = 4, 
               discrete.theta.alpha.values = seq(5, 150, 5),
               discrete.theta.beta.values = seq(5, 150, 5),
               discrete.theta.gibbs = T,
               coords = dat[, c("x", "y")],
               nngp = F,
               space.id = dat$space_id,
               time.id = dat$time_id,
               spacetime.id = dat$spacetime_id,
               verbose = T,
               verbose.iter = 1
)
})


saveRDS(ctm_fit_gibbs, "../output/nngp_test_one_month_wildfire/ctm_fit_disc_gibbs.rds")
#ctm_fit_gibbs <- readRDS("../output/nngp_test_one_month_wildfire/ctm_fit_disc_gibbs.rds")



cat("#########################\n \n \n \n \n \n         TEST 8 \n \n \n \n \n \n#########################")


cv_id_ctm_ord_gibbs <- create_cv(space.id = dat$space_id,
                           time.id = dat$time_id,
                           type = "ordinary")

time_gibbs_cv <- system.time({
ctm_fit_cv_gibbs <- grm_cv(Y = dat$pm_aqs,
                     cv.object = cv_id_ctm_ord_gibbs,
                     X = dat$daily_mean_pm25_tot_ncar,
                     L = model.matrix(~factor(dat$location))[,-1],
                     M = model.matrix(~factor(dat$days))[,-1], 
                     n.iter = 5000, burn = 1000, thin = 4, 
                     discrete.theta.alpha.values = seq(5, 150, 5),
                     discrete.theta.beta.values = seq(5, 150, 5),
                     discrete.theta.gibbs = T,
                     coords = dat[, c("x", "y")],
                     nngp = F,
                     space.id = dat$space_id,
                     time.id = dat$time_id,
                     spacetime.id = dat$spacetime_id,
                     verbose = T,
                     verbose.iter = 1)
})


saveRDS(ctm_fit_cv_gibbs, "../output/nngp_test_one_month_wildfire/ctm_fit_cv_disc_gibbs.rds")
#ctm_fit_cv_gibbs <- readRDS("../output/nngp_test_one_month_wildfire/ctm_fit_cv_disc_gibbs.rds")



#collate and write time results
all_time <- rbind(time_nngp, time_nngp_cv, 
                  time_reg, time_reg_cv, 
                  time_mh, time_mh_cv, 
                  time_gibbs, time_gibbs_cv)

saveRDS(all_time, "../output/nngp_test_one_month_wildfire/all_time.rds")
#all_time <- readRDS(all_time, "../output/nngp_test_one_month_wildfire/all_time.rds")
