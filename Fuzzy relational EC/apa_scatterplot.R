
apa_scatterplot <- function(x, ...){
  UseMethod("apa_scatterplot", data)
}


apa_scatterplot.default <- function(
  x
  , y
  , z = NULL
  , regression_line = TRUE
  , interval = "confidence"
  , z_label = "Moderator"
  , ...
) {

  args <- list(...)

  if(is.null(regression_line) || is.na(regression_line)) regression_line <- FALSE
  
  if(!is.null(z) && !inherits(z, "factor")) stop("Argument 'z' must be a factor.")
  
  if(regression_line) {
    if(is.null(z)) {
      linear_model <- lm(formula = y ~ x)
      z <- factor(rep(1L, length.out = length(x)))
    } else {
      linear_model <- lm(formula = y ~ x * z)
    }
    summary_lm <- summary(linear_model)
  }

  args <- papaja:::defaults(
    args
    , set = list(
      x = x
      , y = y
    )
    , set.if.null = list(
      frame.plot = FALSE
      , pch = c(21,22)
      , bg = rep(c("skyblue3", "skyblue4", "skyblue2"), length.out = nlevels(z))
      , col = rep("white", length.out = nlevels(z))
      , xlab = variable_label(x)
      , ylab = variable_label(y)
      , xlim = range(x, na.rm = TRUE)
      , ylim = range(y, na.rm = TRUE)
    )
  )

  plot.new()
  plot.window(
    xlim = args$xlim
    , ylim = args$ylim
  )
  # do.call("plot", ellipsis)

  # confidence interval
  if(regression_line) {
    
    new_x <- seq(min(x, na.rm = TRUE), max(x, na.rm = TRUE), diff(range(x, na.rm = TRUE)/1e3))
    
    predict_slope <- function(x) {
      as.data.frame(
        predict(
          linear_model
          , newdata = data.frame(
            x = new_x
            , z = x
          )
          , interval = interval
        )
      )
    }
    
    predictions <- lapply(X = levels(z), FUN = predict_slope)
    
    draw_polygon <- function(i) {
      
      rgb_col <- col2rgb(col = args$bg[i])
      polygon(
        x = c(new_x, rev(new_x))
        , y = c(predictions[[i]]$lwr, rev(predictions[[i]]$upr))
        , border = NA
        , col = rgb(r = rgb_col[1, 1], g = rgb_col[2, 1], b = rgb_col[3, 1], alpha = 24, maxColorValue = 255)
      )
      lines(x = new_x, y = predictions[[i]]$lwr, lty = "dashed", col = args$bg[i])
      lines(x = new_x, y = predictions[[i]]$upr, lty = "dashed", col = args$bg[i])
    }
    
    dump <- lapply(seq_along(levels(z)), draw_polygon)
  }
  
  draw_data <- function(i) {
    idx <- (z == levels(z)[i])
    
    points(
      x = x[idx]
      , y = y[idx]
      , pch = args$pch[i]
      , col = args$col[i]
      , bg = args$bg[i]
    )
  }

  lapply(X = seq_along(levels(z)), FUN = draw_data)

  axis(side = 1)
  axis(side = 2)

  # lines(x = new_x, y = predicted$fit, lwd = 3, col = "white")
  if(regression_line) {
    draw_slope <- function(i) {
      lines(
        x = new_x
        , y = predictions[[i]]$fit
        , col = args$bg[i]
      )
    }
    dump <- lapply(
      X = seq_along(levels(z))
      , FUN = draw_slope
    )
    
    
    # legend(
    #   x = "topleft"
    #   , bty = "n"
    #   , legend = substitute(
    #     paste(italic(r)==rsimple,","~italic(p)~pvalue)
    #     , list(
    #       rsimple = papaja::printnum(sign(summary_lm$coefficients[2])*sqrt(summary_lm$r.squared), gt1 = FALSE, digits = 3)
    #       , pvalue = papaja:::add_equals(papaja::printp(summary_lm$coefficients["x", "Pr(>|t|)"]))
    #     )
    #   )
    # )
    # if(nlevels(z) > 1L) {
    #   legend(
    #     x = "topright"
    #     , legend = levels(z)
    #     , pch = args$pch
    #     , pt.bg = args$bg
    #     , lwd = 1
    #     , bty = "n"
    #     , title = z_label
    #   )
    # }
    
  }

  title(
    xlab = args$xlab
    , ylab = args$ylab
  )
}
