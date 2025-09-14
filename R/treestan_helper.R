
plot_regression <- function(x, pars, quantiles = c(.025, .5, .975), pt.col = 1, pt.bg = 1, ...) {
  
  if(missing(pars)) pars <- names(x@parameter_index)
  
  three_stats <- function(x, ...) {
    quantile(x, probs = quantiles, names = FALSE)
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
  
  
  
  
  par(mfrow = c(2, 3), las = 1)
  palette(wesanderson::wes_palette("Zissou1", n = 3, type = "c"))
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
      , col = "grey80"
    )
    arrows(
      x0 = theta[1, , i]
      , x1 = theta[3, , i]
      , y0 = y_marginalized[2, , i]
      # , y1 = y_marginalized[3, , i]
      , length = .02
      , code = 3
      , angle = 90
      , col = "grey80"
    )
    
    lines(
      x = theta_new
      , y = vapply(newdata_predicted, FUN = `[`, i = 2L, j = i, FUN.VALUE = numeric(1L)) * x@standata$lm_y_sd + x@standata$lm_y_m
    )
    lines(x = theta_new, y = vapply(newdata_predicted, FUN = `[`, i = 1L, j = i, FUN.VALUE = numeric(1L)) * x@standata$lm_y_sd + x@standata$lm_y_m, lty = "dashed")
    lines(x = theta_new, y = vapply(newdata_predicted, FUN = `[`, i = 3L, j = i, FUN.VALUE = numeric(1L)) * x@standata$lm_y_sd + x@standata$lm_y_m, lty = "dashed")
    # points(x = theta[, i], y = y_predicted[, i], pch = 16, col = 1)
    

    
    points(x = theta[2, , i], y = y_marginalized[2, , i], pch = 21, col = pt.col, bg = pt.bg)
    axis(side = 1)
    axis(side = 2)
    title(
      xlab = paste("Parameter", names(x@parameter_index)[[i]])
      , ylab = "Marginal EC effect"
    )
  }
}

bayes_factors <- function(x, y, pars = "lm_beta", prior_mean = 0, prior_sd = 2, ...) {
  pars <- match.arg(pars, choices = c("beta", "lm_beta", "lm_zbeta"), several.ok = FALSE)
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
 
