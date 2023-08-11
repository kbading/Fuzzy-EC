# ensure that MPTinR is loaded prior to definining functions in this script:
library(MPTinR)


# We create our own fit_mpt() function to suppress all those nasty messages and
# to add a class atribute:
fit.mpt <- function(...) {
  console_output <- capture.output(y <- MPTinR::fit.mpt(...))
  structure(y, class = "mptinr_model")
}

apa_print.mptinr_model <- function(x, ...) {
  
  list(
    estimate = NULL
    , statistic = NULL
    , full_result = list(
      modelfit = list(
        AIC = paste0("$\\mathrm{AIC} = ", papaja::apa_num(x$information.criteria$AIC), "$")
        , BIC = paste0("$\\mathrm{BIC} = ", papaja::apa_num(x$information.criteria$BIC), "$")
        , G2 = paste0(
          "$G^2("
          , papaja::apa_df(x$goodness.of.fit$df)
          , ") = "
          , papaja::apa_num(x$goodness.of.fit$G.Squared)
          , "$, $p "
          , papaja::apa_p(x$goodness.of.fit$p.value, add_equals = TRUE)
          , "$"
        )
      )
    )
  )
}



# For model comparisons, we create an anova() method
anova.mptinr_model <- function(x, ...) {
  
  model_list <- list(Reference = x, ...)
  
  y <- structure(
    data.frame(
      G.squared = vapply(model_list, FUN = function(x){x$goodness.of.fit$G.Squared}, FUN.VALUE = numeric(1L))
      , df      = vapply(model_list, FUN = function(x){x$goodness.of.fit$df}       , FUN.VALUE = numeric(1L))
      , p.value = vapply(model_list, FUN = function(x){x$goodness.of.fit$p.value}  , FUN.VALUE = numeric(1L))
      , diff.G.squared = NA # c(NA, abs(x$goodness.of.fit$G.Squared - y$goodness.of.fit$G.Squared))
      , diff.df        = NA # c(NA, abs(x$goodness.of.fit$df - y$goodness.of.fit$df))
      , diff.p.value   = NA # rep(NA, 2)
    )
    , class = c("mptinr_comparisons", "anova", "data.frame")
    , heading = "Model comparisons for a family of MPT models"
  )
  
  within(
    y
    , {
      diff.G.squared <- abs(G.squared[[1L]] - G.squared)
      diff.df        <- abs(df[[1L]]        - df)
      diff.p.value   <- pchisq(q = diff.G.squared, df = diff.df, lower.tail = FALSE)
      diff.G.squared[[1L]] <- diff.df[[1L]] <- diff.p.value[[1L]] <- NA
    }
  )
}



apa_print.mptinr_comparisons <- function(x) {
  y <- Map(
    G2   = papaja::apa_num(x$diff.G.squared[-1L])
    , df = papaja::apa_df(x$diff.df[-1L])
    , p  = papaja::apa_p(x$diff.p.value[-1L], add_equals = TRUE)
    , f = function(G2, df, p) {
      paste0("$\\Delta G^2(", df,") = ", G2,"$, $p ", p, "$")
    }
  )
  names(y) <- rownames(x)[-1L]
  y
}

if(!requireNamespace("snowfall", quietly = TRUE)) install.packages("snowfall")

fit.mpt.pb <- function(data, model.filename, restrictions.filename, replicates = 2e3, model_object, ...) {
  
  if(!missing(model_object) && file.exists(model_object)) return(readRDS(model_object))
  
  x <- fit.mpt(
    data = data
    , model.filename = model.filename
    , ...
  )
  
  bootstrap_samples <- gen.data(
    parameter.values = x$parameters$estimates
    , samples = replicates
    , model.filename = model.filename
    , data = data
  )
  
  out <- suppressMessages(fit.mpt(
    data = bootstrap_samples
    , model.filename = model.filename
    , multicore = "individual"
    , sfInit = TRUE
    , nCPU = parallel::detectCores()
    , ...
  ))
  if(!missing(model_object)) saveRDS(out, file = model_object)
  out
}

