
apa_scatterplot <- function(x, ...){
  UseMethod("apa_scatterplot", data)
}


apa_scatterplot.default <- function(
  x
  , y
  , ...
) {

  args <- list(...)

  regression_line <- TRUE # todo: provide a parameter

  if(regression_line) {
    linear_model <- lm(formula = y ~ x)
  }

  summary_lm <- summary(linear_model)

  args <- papaja:::defaults(
    args
    , set = list(
      x = x
      , y = y
    )
    , set.if.null = list(
      frame.plot = FALSE
      , pch = 21
      , bg = "grey40"
      , col = "white"
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
  new_x <- seq(min(x, na.rm = TRUE), max(x, na.rm = TRUE), diff(range(x, na.rm = TRUE)/1e3))

 predicted <- predict(
   linear_model
   , newdata = data.frame(
     x = new_x
    )
   , se.fit = TRUE
  )
  lower_boundary <- predicted$fit + predicted$se.fit * qt(p = .025, df = predicted$df)
  upper_boundary <- predicted$fit + predicted$se.fit * qt(p = .975, df = predicted$df)

  polygon(
    x = c(new_x, rev(new_x))
    , y = c(lower_boundary, rev(upper_boundary))
    , border = NA
    , col = "grey80"
  )

  lines(x = new_x, y = lower_boundary, lty = "dashed", col = "grey20")
  lines(x = new_x, y = upper_boundary, lty = "dashed", col = "grey20")

  points(
    x = x
    , y = y
    , pch = args$pch
    , col = args$col
    , bg = args$bg
  )

  axis(side = 1)
  axis(side = 2)

  # lines(x = new_x, y = predicted$fit, lwd = 3, col = "white")
  lines(x = new_x, y = predicted$fit)


  title(
    xlab = args$xlab
    , ylab = args$ylab
  )



  legend(
    x = "topleft"
    , bty = "n"
    , legend = substitute(
      paste(italic(r)==rsimple,","~italic(p)~pvalue)
      , list(
        rsimple = papaja::printnum(sign(summary_lm$coefficients[2])*sqrt(summary_lm$r.squared), gt1 = FALSE, digits = 3)
        , pvalue = papaja:::add_equals(papaja::printp(summary_lm$coefficients["x", "Pr(>|t|)"]))
      )
    )
  )
}
