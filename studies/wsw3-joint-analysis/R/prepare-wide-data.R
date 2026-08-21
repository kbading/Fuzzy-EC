
project_root <- rprojroot::find_rstudio_root_file()
study_folder <- file.path(project_root, "studies", "wsw3-joint-analysis")

data <- list(
  main = readRDS(file.path(project_root, "studies", "wsw3-main", "data", "data.rds"))
  , pilot2 = readRDS(file.path(project_root, "studies", "wsw3-p2", "data", "data.rds"))          
)

x <- data$main
nm <- "main"

TreeBUGS::withinSubjectEQN(
  file.path(study_folder, "WSW_exp3_hierarchical.eqn")
  , save = file.path(study_folder, "WSW_exp3_wide.eqn")
  , labels = paste0("rating", 1:8)
  , constant = c("D", "C", "d", "G", "b")
)
mpt_levels <- MPTinR::check.mpt(file.path(study_folder, "WSW_exp3_wide.eqn"))$eqn.order.categories
x <- data$main
mpt_data_hierarchical <- Map(
  x = data
  , nm = names(data)
  , f = function(x, nm) {
    x$rating$sid <- paste0(nm, "_", x$rating$sid)
    x$rating$study <- nm
    x$memory$sid <- paste0(nm, "_", x$memory$sid)
    
    y <- merge(
      subset(x$rating, select = c(sid, cs, evaluative_rating, task_focus, study, us_valence))
      , subset(x$memory, select = c(sid, cs, mpt_response))
    ) |>
      within({
        mpt_response_rating <- paste0("rating", as.character(evaluative_rating), "_", mpt_response) |>
          factor(levels = mpt_levels)
        
      })
    
    mpt_data <- as.data.frame(unclass(table(y$sid, y$mpt_response_rating)))
    mpt_data$sid <- rownames(mpt_data)
    
    
    
    agg <- aggregate(evaluative_rating ~ sid + us_valence + task_focus + study, data = y, FUN = mean) |>
      tidyr::pivot_wider(names_from = "us_valence", values_from = "evaluative_rating") |>
      within({ec_effect <- positive - negative})
    # merge(x$mpt_data_hierarchical, agg)
    merge(mpt_data, agg, sort = FALSE)
  }
) |>
  do.call(what = "rbind") |>
  within({
    study <- factor(study)
    sid <- factor(sid)
    task_focus <- factor(task_focus)
  })

dir.create(file.path(project_root, "studies", "wsw3-joint-analysis", "data"), showWarnings = FALSE)
saveRDS(
  list(mpt_data_hierarchical = mpt_data_hierarchical)
  , file = file.path(project_root, "studies", "wsw3-joint-analysis", "data", "data-wide.rds")
)



