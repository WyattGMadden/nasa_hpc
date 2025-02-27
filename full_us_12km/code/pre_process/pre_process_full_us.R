set.seed(1001001)

#get CTM, grid.info, OBS, OBS.info
#process observations
load("../../data/howard_uploaded/Grid_PM25.RData")

obs <- OBS
names(obs) <- tolower(names(obs))

obs$date <- as.Date(obs$date)
obs$space_id <- as.numeric(as.factor(obs$aqs_site_id))
obs$time_id <- as.numeric(as.factor(as.numeric(obs$date)))
obs$spacetime_id <- as.numeric(substr(obs$date, 6, 7))


#remove space ids that don't show up in every spacetime
unique_space_spacetime_ids <- unique(paste(obs$space_id, obs$spacetime_id, sep = "-"))
all_possible_ids <- paste(rep(1:max(obs$space_id), times = max(obs$spacetime_id)),
                          rep(1:max(obs$spacetime_id), each = max(obs$space_id)),
                          sep = "-")

not_present <- all_possible_ids[!(all_possible_ids %in% unique_space_spacetime_ids)]
offending_space_ids <- sub("-.*$", "", not_present)
obs <- obs[!(obs$space_id %in% as.integer(offending_space_ids)), ]


#remove space ids with less than 50 observations, for cv purposes
space_counts <- table(obs$space_id)
offending_space_ids <- space_counts[space_counts < 100] |>
    names() |>
    as.integer()
obs <- obs[!(obs$space_id %in% offending_space_ids), ]

obs$space_id <- as.numeric(as.factor(obs$aqs_site_id))
obs$time_id <- as.numeric(as.factor(as.numeric(obs$date)))
obs$spacetime_id <- as.numeric(substr(obs$date, 6, 7))




saveRDS(obs, "../../data/created/obs.rds")
obs <- readRDS("../../data/created/obs.rds")
obs


#process preds
grid.info <- grid.info
names(grid.info) <- tolower(names(grid.info))
grid.info <- grid.info[, c("grid_cell", "x", "y")]

preds <- CTM
names(preds) <- tolower(names(preds))

preds$date <- as.Date(preds$date)
preds <- preds[preds$date %in% obs$date, ]
preds$space_id <- preds$grid_cell
preds$time_id <- as.numeric(factor(as.numeric(preds$date),
                                   levels = levels(as.factor(as.numeric(obs$date)))))
preds$spacetime_id <- as.numeric(substr(preds$date, 6, 7))

#left join grid.info on preds
preds <- merge(preds, 
               grid.info, 
               by = "grid_cell",
               all.x = TRUE)

saveRDS(preds, "../../data/created/preds.rds")


