
library(TreeBUGS)

project_root <- rprojroot::find_rstudio_root_file()
study_folder <- file.path(project_root, "studies", "wsw1")
# 
# data_list <- readRDS(file.path(study_folder, "data", "data.rds"))
# mpt_data_hierarchical <- merge(data_list$mpt_data_hierarchical, data_list$rating_wide, sort = FALSE)
# stopifnot(all(mpt_data_hierarchical$sid == data_list$mpt_data_hierarchical$sid))
# 
# mpt_data_hierarchical <- within(
#   mpt_data_hierarchical
#   , {
#     ec_rating_first <- ec_effect * (task_order == "Rating first")
#     ec_memory_first <- ec_effect * (task_order == "Memory first")
#   }
# )
# 
# 
# model <- traitMPT(
#   , eqnfile = file.path(study_folder, "WSW_exp2_hierarchical.eqn")
#   , data = mpt_data_hierarchical
#   , n.adapt  = 2e4L
#   , n.burnin = 1e5L
#   , n.iter   = 2e5L
#   , n.thin   = 2e1L
#   , n.chains = 4e0L
#   , ppp      = 5e3L
#   , restrictions = list("G = 0.25")
#   , covData = subset(mpt_data_hierarchical, select = c("task_order", "ec_rating_first", "ec_memory_first"))
#   , predStructure = list("a b D d C; task_order", "a b D d C; ec_rating_first", "a b D d C; ec_memory_first")
#   , predType = c("f", "c", "c")
#   , IVprec = "dgamma(.5,.5)"
# )

dir.create(file.path(study_folder, "model-objects"), showWarnings = FALSE)
saveRDS(list(), file = file.path(study_folder, "model-objects", "trait-mpt-with-ec.rds"))
