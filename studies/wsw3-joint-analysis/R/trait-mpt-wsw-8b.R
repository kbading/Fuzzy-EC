
library(TreeBUGS)

project_root <- rprojroot::find_rstudio_root_file()
study_folder <- file.path(project_root, "studies", "wsw3-joint-analysis")

mpt_data_hierarchical <- readRDS(file.path(study_folder, "data", "data.rds"))$mpt_data_hierarchical |>
  within({
    study_task_focus <- interaction(study, task_focus, sep = "_")
  })


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
  , covData = subset(mpt_data_hierarchical, select = c("study_task_focus"))
  , predStructure = list("a b D d_positive d_negative C_positive C_negative ; task_order")
  , predType = c("f")
  , IVprec = "dgamma(.5,.5)"
)


dir.create(file.path(study_folder, "model-objects"), showWarnings = FALSE)
saveRDS(model, file = file.path(study_folder, "model-objects", "trait-mpt-wsw-8b.rds"))
