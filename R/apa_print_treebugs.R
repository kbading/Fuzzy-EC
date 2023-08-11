

apa_print.summary.traitMPT <- function(
  x
  , parameters = "mean"
  , ...) {

  extract_treebugs <- function(x, ...) {
    x <- as.data.frame(x, stringsAsFactors = FALSE)
    x$Term <- gsub(rownames(x), pattern = "mean_", replacement = "", fixed = TRUE)
    x$estimate <- x$Mean
    x$conf.int <- lapply(apply(X = x[, c("2.5%", "97.5%"), drop = FALSE], MARGIN = 1, list), unlist)
    x <- x[, c("Term", "estimate", "conf.int")]
    attr(x$conf.int, "conf.level") <- .95
    rownames(x) <- NULL

    canonical_x <- papaja:::canonize(x, est_label = "$M$")
    beautiful_x <- papaja:::beautify(canonical_x, ...)
    variable_labels(beautiful_x$term) <- "Parameter"
    beautiful_x
  }

  beautiful_x <- extract_treebugs(x$groupParameters[[parameters]], digits = 3L, gt1 = FALSE)

  if(parameters == "rho") {
    beautiful_x$term[] <- gsub(pattern = "rho[", replacement = "$\\rho_{", beautiful_x$term, fixed = TRUE)
    beautiful_x$term[] <- gsub(pattern = "]", replacement = "}$", beautiful_x$term, fixed = TRUE)
  }

  beautiful_x
}


apa_fit <- function(x) {
  with(
    as.list(x$summary$fitStatistics$overall)
    , list(
      T1 = paste0(
        "$T_1^{\\mathrm{observed}} = ", papaja::apa_num(T1.observed)
        , "$, $T_1^{\\mathrm{expected}} = ", papaja::apa_num(T1.predicted)
        , "$, $p  ", papaja::apa_p(p.T1, add_equals = TRUE)
        , "$"
      )
      , T2 = paste0(
        "$T_2^{\\mathrm{observed}} = ", papaja::apa_num(T2.observed)
        , "$, $T_2^{\\mathrm{expected}} = ", papaja::apa_num(T2.predicted)
        , "$, $p  ", papaja::apa_p(p.T2, add_equals = TRUE)
        , "$"
      )
    )
  )
}