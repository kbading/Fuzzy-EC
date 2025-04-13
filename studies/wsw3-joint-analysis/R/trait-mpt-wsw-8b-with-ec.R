
library(TreeBUGS)

project_root <- rprojroot::find_rstudio_root_file()
study_folder <- file.path(project_root, "studies", "wsw3-joint-analysis")

mpt_data_hierarchical <- readRDS(file.path(study_folder, "data", "data.rds"))$mpt_data_hierarchical |>
  within({
    two_way_study_focus <- factor(as.integer(task_focus) == as.integer(factor(study)), levels = c(T, F), labels = c("same", "different"))
    ec_age_focus     <- ec_effect * (task_focus == "age")
    ec_valence_focus <- ec_effect * (task_focus == "valence")
  })

with(mpt_data_hierarchical, ftable(task_focus, study, two_way_study_focus))

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
  , covData = subset(mpt_data_hierarchical, select = c("study", "task_focus", "two_way_study_focus", "ec_age_focus", "ec_valence_focus"))
  , predStructure = list("a b D d_positive d_negative C_positive C_negative; study task_focus two_way_study_focus ec_age_focus ec_valence_focus")
  , predType = c("f", "f", "f", "c", "c")
)
summary(model)
dir.create(file.path(study_folder, "model-objects"), showWarnings = FALSE)
saveRDS(model, file = file.path(study_folder, "model-objects", "trait-mpt-wsw-8b-with-ec.rds"))
