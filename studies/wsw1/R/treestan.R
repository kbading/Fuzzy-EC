
library(TreeStan)
treestan_options(refresh = 0)

project_root <- rprojroot::find_rstudio_root_file()
study_folder <- file.path(project_root, "studies", "wsw1")

source(file.path(project_root, "R", "treestan_helper.R"))
data_list <- readRDS(file.path(study_folder, "data", "data.rds"))

mpt_data_hierarchical <- merge(
  data_list$mpt_data_hierarchical
  , data_list$rating_wide
  , sort = FALSE
)
stopifnot(all(mpt_data_hierarchical$sid == data_list$mpt_data_hierarchical$sid))

model <- TreeStan::fit_mpt(
  eqn_file = file.path(study_folder, "WSW_pilot_hierarchical.eqn")
  , restrictions = list(G = 1/4)
  , data = mpt_data_hierarchical
  # , formula = ~ task_focus
  , lm_y = mpt_data_hierarchical$ec_effect
  , warmup  = 2000
  , iter    = 4000
  , chains  =    8
  # , control = list(
  #   adapt_delta = .9
  #   , stepsize  = .07 # initial stepsize
  # )
)
# shinystan::launch_shinystan(model)
saveRDS(model, file = file.path(study_folder, "model-objects", "treestan.rds"))
# par(mfrow = c(2, 3))
# plot_regression(model, pt.col = "grey90")
# bayes_factors(model)

