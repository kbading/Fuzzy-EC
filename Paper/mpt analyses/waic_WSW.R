
library(TreeBUGS)

study_folder <- file.path(
  rprojroot::find_rstudio_root_file()
  , "Study - Relational asymmetry"
  , "Pilot (project seminar)"
)


model_files <- list.files(path = file.path(study_folder, "model_objects"), pattern = "^mpt-model", full.names = TRUE)

# waic <- vector(mode = "list", length = length(model_files))
waic <- model_files |>
  lapply(readRDS) |> 
  lapply(
    TreeBUGS::WAIC
    , n.adapt = 5e3
    , n.chains = parallel::detectCores()
    , n.iter = 1e5
    , n.thin = 1e1
    , summarize = FALSE
  )
names(waic) <- basename(model_files) |>
  gsub(pattern = "^mpt-|\\.rds$", replacement = "") |>
  gsub(pattern = "-", replacement = "_")

saveRDS(waic, file = file.path(study_folder, "model_objects", "waic.rds"))

