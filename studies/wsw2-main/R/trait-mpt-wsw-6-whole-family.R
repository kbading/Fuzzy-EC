
library(TreeBUGS)

project_root <- rprojroot::find_rstudio_root_file()
study_folder <- file.path(project_root, "studies", "wsw2-main")

mpt_data_hierarchical <- readRDS(file.path(study_folder, "data", "data.rds"))$mpt_data_hierarchical


baseline_restrictions <- list("G = 0.25") |> HMMTreeC::legacy_restrictions()

hypothesis_restrictions <- list(
  baseline         = list()
  , a_eq5          = list(a = .5)
  , b_eq5          = list(b = .5)
  , d_eq0          = list(d = 0)
  , C_eq0          = list(C = 0)
  , D_eq0          = list(D = 0)
) |> HMMTreeC::legacy_restrictions()

model <- readLines(file.path(project_root, "model-equations", "wsw-6.eqn"))

models <- Map(
  x = hypothesis_restrictions
  , f = function(x) {
    
    if(!requireNamespace("TreeBUGS", quietly = TRUE)) install.packages("TreeBUGS")
    
    all_parameters <- c("a", "b", "D", "d", "C")
    fixed_parameters <- names(x)
    
    temporary_file <- tempfile(fileext = ".eqn")
    writeLines(model, con = temporary_file)
    
    y <- TreeBUGS::traitMPT(
      , eqnfile = temporary_file
      , data = mpt_data_hierarchical
      , n.adapt  = 2e4L
      , n.burnin = 1e5L
      , n.iter   = 2e5L
      , n.thin   = 6e1L
      , n.chains =   6L
      , ppp      = 5e3L
      , restrictions = c(baseline_restrictions, x)
      , covData = subset(mpt_data_hierarchical, select = "task_order")
      , predStructure = list(paste0(paste(setdiff(all_parameters, fixed_parameters), collapse = " "), " ; task_order"))
      , predType = "f"
    )
    unlink(temporary_file)
    y
  }
)
dir.create(file.path(study_folder, "model-objects"), showWarnings = FALSE)
saveRDS(models, file = file.path(study_folder, "model-objects", "trait-mpt-wsw-6-whole-family.rds"))
