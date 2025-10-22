
library(TreeBUGS)
library(methexp)
library(parallel)

project_root <- rprojroot::find_rstudio_root_file()
study_folder <- file.path(project_root, "studies", "wsw2-main")


cl <- makeForkCluster(parallel::detectCores())

setDefaultCluster(cl)

waics <- clusterMap(
  x = readRDS(file = file.path(study_folder, "model-objects", "trait-mpt-wsw-6-whole-family.rds"))
  , fun = function(x) {
    TreeBUGS::WAIC(x,  n.adapt = 2e4L, n.chains = 6L, n.iter = 1e5L, n.thin = 1e2L)
  }
  , .scheduling = "dynamic"
)
stopCluster(cl)

saveRDS(waics, file = file.path(study_folder, "model-objects", "waic-wsw-6.rds"))
