
library(TreeStan)
treestan_options(refresh = 0)

project_root <- rprojroot::find_rstudio_root_file()
study_folder <- file.path(project_root, "studies", "wsw2-main")


data_list <- readRDS(file.path(study_folder, "data", "data.rds"))

mpt_data_hierarchical <- merge(
  data_list$mpt_data_hierarchical
  , data_list$rating_wide
  , sort = FALSE
)
stopifnot(all(mpt_data_hierarchical$sid == data_list$mpt_data_hierarchical$sid))
contrasts(mpt_data_hierarchical$task_order) <- "contr.sum"

model <- TreeStan::fit_mpt(
  eqn_file = file.path(study_folder, "WSW_exp2_hierarchical.eqn")
  , restrictions = list(G = 1/4)
  , data = mpt_data_hierarchical
  , formula = ~ task_order
  , lm_y = mpt_data_hierarchical$ec_effect
  , warmup  = 2e3L
  , iter    = 4e3L
  , chains  = 8e0L
  , cores   = parallel::detectCores()
  , refresh = if(interactive()) 100 else 0
)
# shinystan::launch_shinystan(model)
saveRDS(model, file = file.path(study_folder, "model-objects", "treestan.rds"))
# par(mfrow = c(2, 3))
# plot_regression(model, pt.col = "grey90", pt.bg = mpt_data_hierarchical$task_order)
# bayes_factors(model, "beta")
# bayes_factors(model, "lm_beta")

