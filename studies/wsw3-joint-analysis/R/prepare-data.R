
project_root <- rprojroot::find_rstudio_root_file()

data <- list(
  main = readRDS(file.path(project_root, "studies", "wsw3-main", "data", "data.rds"))
  , pilot2 = readRDS(file.path(project_root, "studies", "wsw3-p2", "data", "data.rds"))          
)

mpt_data_hierarchical <- Map(
  x = data
  , nm = names(data)
  , f = function(x, nm) {
    x$rating$sid <- paste0(nm, "_", x$rating$sid)
    x$rating$study <- nm
    agg <- aggregate(evaluative_rating ~ sid + us_valence + task_focus + study, data = x$rating, FUN = mean) |>
      tidyr::pivot_wider(names_from = "us_valence", values_from = "evaluative_rating") |>
      within({ec_effect <- positive - negative})
    x$mpt_data_hierarchical$sid <- paste0(nm, "_", x$mpt_data_hierarchical$sid)
    # merge(x$mpt_data_hierarchical, agg)
    merge(x$mpt_data_hierarchical, agg, sort = FALSE)
  }
) |>
  do.call(what = "rbind") |>
  within({
    study <- factor(study)
    sid <- factor(sid)
  })

dir.create(file.path(project_root, "studies", "wsw3-joint-analysis", "data"), showWarnings = FALSE)
saveRDS(
  list(mpt_data_hierarchical = mpt_data_hierarchical)
  , file = file.path(project_root, "studies", "wsw3-joint-analysis", "data", "data.rds")
)



