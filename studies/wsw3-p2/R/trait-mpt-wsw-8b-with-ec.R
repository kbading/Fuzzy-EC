
library(TreeBUGS)

project_root <- rprojroot::find_rstudio_root_file()
study_folder <- file.path(project_root, "studies", "wsw3-p2")

data_list <- readRDS(file.path(study_folder, "data", "data.rds"))

mpt_data_hierarchical <- merge(data_list$mpt_data_hierarchical, data_list$rating_wide, sort = FALSE)
stopifnot(all(mpt_data_hierarchical$sid == data_list$mpt_data_hierarchical$sid))

mpt_data_hierarchical <- within(
  mpt_data_hierarchical
  , {
    ec_age_focus     <- ec_effect * (task_focus == "age")
    ec_valence_focus <- ec_effect * (task_focus == "valence")
  }
)
rownames(mpt_data_hierarchical) <- mpt_data_hierarchical$sid

model <- traitMPT(
  , eqnfile = file.path(project_root, "model-equations", "wsw-8b.eqn")
  , data = mpt_data_hierarchical
  , n.adapt  = 2e4L
  , n.burnin = 1e5L
  , n.iter   = 2e5L
  , n.thin   = 2e1L
  , n.chains = 4e0L
  , ppp      = 5e3L
  , restrictions = list("G = 0.125")
  , covData = subset(mpt_data_hierarchical, select = c("task_focus", "ec_age_focus", "ec_valence_focus"))
  , predStructure = list("a b D d_positive d_negative C_positive C_negative ; task_focus ec_age_focus ec_valence_focus")
  , predType = c("f", "c", "c")
)


dir.create(file.path(study_folder, "model-objects"), showWarnings = FALSE)
saveRDS(model, file = file.path(study_folder, "model-objects", "trait-mpt-wsw-8b-with-ec.rds"))
