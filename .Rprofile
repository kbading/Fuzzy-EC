options(knitr.progress.fun = function(total, labels) {
  id = cli::cli_progress_bar(
    total = total, .auto_close = FALSE
  )
  list(
    update = function(i) {
      cli::cli_progress_update(id = id)
    },
    done = function() {
      cli::cli_process_done(id)
    }
  )
})

methexp_bib <- function(update = FALSE) {
  
  update <- isTRUE(update)
  
  dest <- file.path(rprojroot::find_rstudio_root_file(), ".methexp.bib")
  
  if (!file.exists(dest) || update) {
    download.file(
      "https://gist.githubusercontent.com/mariusbarth/9bd91e9f88c5db012c70eaf13a154747/raw/methexp.bib"
      , destfile = dest
    )
  }
  dest
}
