
library(TreeBUGS)
library(methexp)
library(parallel)

project_root <- rprojroot::find_rstudio_root_file()
study_folder <- file.path(project_root, "studies", "wsw2-main")

mpt_data_hierarchical <- readRDS(file.path(study_folder, "data", "data.rds"))$mpt_data_hierarchical


baseline_restrictions <- list("G = 0.25")

hypothesis_restrictions <- list(
  baseline         = list()
  , a_eq5          = list(a = .5)
  , b_eq5          = list(b = .5)
  , d_positive_eq0 = list(d_positive = 0)
  , d_negative_eq0 = list(d_negative = 0)
  , d_eq0          = list(d_positive = 0, d_negative = 0)
  , C_positive_eq0 = list(C_positive = 0)
  , C_negative_eq0 = list(C_negative = 0)
  , C_eq0          = list(C_positive = 0, C_negative = 0)
  , D_eq0          = list(D = 0)
)

model <- readLines(file.path(project_root, "model-equations", "wsw-8b.eqn"))

cl <- methexp_cluster(
  master = "134.95.17.62"
  , servants = paste0("134.95.17.", 62:65)
  , user = "mariusbarth"
  , cores = 1L
)

setDefaultCluster(cl)
clusterExport(varlist = c("baseline_restrictions", "mpt_data_hierarchical", "model"))

models <- clusterMap(
  x = hypothesis_restrictions
  , fun = function(x) {
    
    if(!requireNamespace("TreeBUGS", quietly = TRUE)) install.packages("TreeBUGS")
    
    all_parameters <- c("a", "b", "D", "d_positive", "d_negative", "C_positive", "C_negative")
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
      , restrictions = c(baseline_restrictions, as.list(paste0(names(x), "=", unname(x))))
      , covData = subset(mpt_data_hierarchical, select = "task_order")
      , predStructure = list(paste0(paste(setdiff(all_parameters, fixed_parameters), collapse = " "), " ; task_order"))
      , predType = "f"
    )
    unlink(temporary_file)
    y
  }
)
stopCluster(cl)
dir.create(file.path(study_folder, "model-objects"), showWarnings = FALSE)
saveRDS(models, file = file.path(study_folder, "model-objects", "trait-mpt-wsw-8b-whole-family.rds"))
