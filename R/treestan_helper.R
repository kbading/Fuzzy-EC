
plot_regression <- function(x, pars, ylim = c(-2, 6), ...) {
  
  parameter_labels <- list(
    D   = expression(paste("Parameter"~italic(D), ""))
    , C = expression(paste("Parameter"~italic(C), ""))
    , d = expression(paste("Parameter"~italic(d), ""))
    , a = expression(paste("Parameter"~italic(a), ""))
    , b = expression(paste("Parameter"~italic(b), ""))
    , G = expression(paste("Parameter"~italic(g), ""))
  )
  
  parameter_meanings <- list(
    D   = "CS recognition memory"
    , C = "US identity memory"
    , d = "US valence memory"
    , a = "Guessing positive"
    , b = "Guessing old"
    , G = "Guessing correct US identity"
  )
  
  # use the new plot.treestanfit() from TreeStan v0.0.1.9002:
  plot(
    x
    , pars = pars
    , ylim = ylim
    , xlab = parameter_labels
    , main = parameter_meanings
    , ...
  ) 
 
}

bayes_factors <- function(x, y, pars = "lm_beta", prior_mean = 0, prior_sd = 2, ...) {
  pars <- match.arg(pars, choices = c("beta", "lm_beta", "lm_zbeta", "lm_alpha_tilde"), several.ok = FALSE)
  # lm_beta: slopes (MPT parameter predicting EC)
  samples <- rstan::extract(x, pars = pars)[[1L]]
  
  log_dens_at_0 <- t(apply(
    samples
    , MARGIN = seq_along(dim(samples))[-1L]
    , FUN = function(x) {
      logspline::dlogspline(logspline::logspline(x), q = 0, log = TRUE)
    }
    , simplify = TRUE
  ))
  
  if(missing(y) || is.null(y)) {
    prior_dens_at_0 <- dnorm(0, mean = prior_mean, sd = prior_sd, log = TRUE)
  } else {
    samples <- rstan::extract(y, pars = pars)[[1L]]
    
    prior_dens_at_0 <- t(apply(
      samples
      , MARGIN = seq_along(dim(samples))[-1L]
      , FUN = function(x) {
        logspline::dlogspline(logspline::logspline(x), q = 0, log = TRUE)
      }
      , simplify = TRUE
    ))
  }
  
  if(identical(pars, "beta")) {
    term <- rep(colnames(x@standata$X), each = length(x@parameter_index))  
  } else {
    term <- "slope"
  }
  
  
  structure(
    data.frame(
      parameter = names(sort(x@parameter_index))
      , term    = term
      , BF_01   = as.numeric(exp(log_dens_at_0 - prior_dens_at_0))
      , BF_10   = as.numeric(exp(prior_dens_at_0 - log_dens_at_0))
      , log_BF_10 = as.numeric(prior_dens_at_0 - log_dens_at_0)
    )
    , class = c("treestan_bfs", "data.frame")
  )
}


apa_print.treestan_bfs <- function(x, ...) {
  x$parameter <- factor(x$parameter, levels = unique(x$parameter))
  split(x, x$parameter, lex.order = FALSE) |>
    lapply(function(x){
      label <- ifelse(x$log_BF_10 < 0, "\\mathit{BF}_{01}", "\\mathit{BF}_{10}")
      statistic <- ifelse(x$log_BF_10 < 0, exp(-x$log_BF_10), exp(x$log_BF_10))
      statistic <- ifelse(statistic > 1000, "> 1,000", apa_num(statistic, digits = 2L))
      
      paste0("$", label, " ", papaja::add_equals(statistic), "$")
    })
}
 
apa_print.treestanfit <- function(x, part = c("lm", "mpt"), ...) {
  
  part <- match.arg(part)
  three_stats <- function(x, conf.int = .95) {
    qs <- quantile(x, probs = .5 + c(-1, 1) * conf.int/2, names = FALSE)
    c(qs[1L], mean(x), qs[2L])
  }
  
  if(part == "lm") {
    est_pars <-  "lm_beta_star"
    bf_pars <- "lm_beta"
    prior_sd <- 2
    est_label <- "$b^*$"
  }
  if(part == "mpt") {
    est_pars <-  "beta"
    bf_pars <- "beta"
    prior_sd <- 1
    est_label <- "$\\delta$"
  }
  
  lm_beta_star <- rstan::extract(x, pars = est_pars)[[1L]]
  if(part == "lm") {
    estimates <- apply(lm_beta_star, MARGIN = 2L, FUN = three_stats, simplify = FALSE)
  } else {
    estimates <- apply(lm_beta_star[, 2, ], MARGIN = 2L, FUN = three_stats, simplify = FALSE)
  }

  canonical_table <- data.frame(
    term = paste0("$", names(x@parameter_index), "$")
    , estimate = vapply(estimates, FUN = `[[`, i = 2, FUN.VALUE = numeric(1L))
  )
  canonical_table$conf.int <- lapply(estimates, function(x){x[c(1, 3)]})
  bfs <- subset(bayes_factors(x, pars = bf_pars, prior_sd = prior_sd), term != "(Intercept)")
  bfs$BF_10 <- ifelse(bfs$BF_10 > 1000, "> 1,000", apa_num(bfs$BF_10))
  canonical_table <- cbind(canonical_table, statistic = bfs$BF_10)
  tinylabels::variable_labels(canonical_table) <- list(
    term = "Parameter"
    , estimate = est_label
    , conf.int = "95\\% CI"
    , statistic = "$\\mathit{BF}_{10}$"
  )
  beautiful_table <- papaja:::beautify(canonical_table)
  beautiful_table$term[] <- canonical_table$term
  beautiful_table <- subset(beautiful_table, term != "$G$")
  rownames(beautiful_table) <- NULL
  
  lm_part <- papaja::glue_apa_results(
    beautiful_table
    , term_names = papaja:::strip_math_tags(beautiful_table$term)
    , est_glue = papaja:::est_glue(beautiful_table)
    , stat_glue = papaja:::stat_glue(beautiful_table)
  )
  return(lm_part)
}
