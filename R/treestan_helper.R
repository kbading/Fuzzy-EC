
plot_regression <- function(x, pars, quantiles = c(.025, .5, .975), pt.col = "black", ...) {
  
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
  
  if(missing(pars)) pars <- names(x@parameter_index)
  
  three_stats <- function(x, ...) {
    qs <- quantile(x, probs = quantiles, names = FALSE)
    c(qs[1L], mean(x), qs[3L])
  }
  
  y_marginalized <- rstan::extract(x, "y_marginalized")[[1L]] |>
    apply(MARGIN = 2:3, FUN = three_stats)
  
  y_predicted <- rstan::extract(x, "predictions")[[1]] |>
    apply(MARGIN = 2:3, FUN = three_stats)
  
  theta <- rstan::extract(x, "theta")[[1]] |>
    apply(MARGIN = 2:3, FUN = three_stats)
  
  theta_m  <- rstan::extract(x, "theta_m" )[[1L]]
  theta_sd <- rstan::extract(x, "theta_sd")[[1L]]
  theta_c  <- rstan::extract(x, "theta_c" )[[1L]]
  lm_beta  <- rstan::extract(x, "lm_beta" )[[1L]]
  
  theta_new <- seq(0, 1, length = 1e2L)
  newdata_y_predict <- lapply(
    X = theta_new
    , FUN = function(x, mean, lm_beta) {
      theta_c <- (x - mean) # / sd
      theta_c * lm_beta
    }
    , mean = theta_m
    # , sd = theta_sd
    , lm_beta = lm_beta
  )
  
  newdata_predicted <- lapply(newdata_y_predict, function(x) {
    apply(X = x, MARGIN = 2, FUN = three_stats)
  })
  
  
  
  

  for(i in x@parameter_index[pars]) {
    plot.new()
    plot.window(xlim = c(0, 1), ylim = c(-2, 6))
    
    arrows(
      x0 = theta[2, , i]
      , y0 = y_marginalized[1, , i]
      , y1 = y_marginalized[3, , i]
      , length = .02
      , code = 3
      , angle = 90
      , col = "grey70"
    )
    arrows(
      x0 = theta[1, , i]
      , x1 = theta[3, , i]
      , y0 = y_marginalized[2, , i]
      # , y1 = y_marginalized[3, , i]
      , length = .02
      , code = 3
      , angle = 90
      , col = "grey70"
    )
    
    lines(
      x = theta_new
      , y = vapply(newdata_predicted, FUN = `[`, i = 2L, j = i, FUN.VALUE = numeric(1L)) * x@standata$lm_y_sd + x@standata$lm_y_m
    )
    lines(x = theta_new, y = vapply(newdata_predicted, FUN = `[`, i = 1L, j = i, FUN.VALUE = numeric(1L)) * x@standata$lm_y_sd + x@standata$lm_y_m, lty = "dashed")
    lines(x = theta_new, y = vapply(newdata_predicted, FUN = `[`, i = 3L, j = i, FUN.VALUE = numeric(1L)) * x@standata$lm_y_sd + x@standata$lm_y_m, lty = "dashed")
    # points(x = theta[, i], y = y_predicted[, i], pch = 16, col = 1)
    

    mf <- x@model_frame
    if(ncol(mf) > 0) {
      bg <- x@model_frame[[1L]]
    } else {
      bg <- 1
    }
    
    points(x = theta[2, , i], y = y_marginalized[2, , i], pch = 21, col = pt.col, bg = bg)
    axis(side = 1)
    axis(side = 2)
    p <- names(x@parameter_index)[[i]]
    
    title(
      # main   = parameter_meanings[[p]]# paste("Parameter", names(x@parameter_index)[[i]])
      xlab = parameter_labels[[p]]
      , ylab = "Marginal EC effect"
    )
    title(
      main   = parameter_meanings[[p]]# paste("Parameter", names(x@parameter_index)[[i]])
      , line = 0
    )
  }
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
