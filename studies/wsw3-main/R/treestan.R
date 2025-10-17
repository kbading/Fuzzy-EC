
library(TreeStan)
treestan_options(refresh = 0)

project_root <- rprojroot::find_rstudio_root_file()
study_folder <- file.path(project_root, "studies", "wsw3-main")

mpt_data_hierarchical <- readRDS(file.path(study_folder, "data", "data.rds"))$mpt_data_hierarchical
contrasts(mpt_data_hierarchical$task_focus) <- "contr.sum"

model <- TreeStan::fit_mpt(
  eqn_file = file.path(study_folder, "WSW_exp3_hierarchical.eqn")
  , restrictions = list(G = 1/8)
  , data = mpt_data_hierarchical
  , formula = ~ task_focus
  , lm_y = mpt_data_hierarchical$ec_effect
  , warmup  = 2e3L
  , iter    = 4e3L
  , chains  = 8e0L
  , cores   = parallel::detectCores()
  , refresh = if(interactive()) 100 else 0
)

dir.create(file.path(study_folder, "model-objects"), showWarnings = FALSE)
saveRDS(model, file = file.path(study_folder, "model-objects", "treestan.rds"))
