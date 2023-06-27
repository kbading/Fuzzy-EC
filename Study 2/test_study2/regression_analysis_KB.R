
N <- 30

data <- expand.grid(
  us_valence = factor(c("positive", "negative"), levels = c("positive", "negative"))
  , id = seq_len(N)
) |>
  within({
    # predictors
    D <- rep(rnorm(N), each = 2)
    C <- rnorm(N * 2)
    a <- rep(rnorm(N), each = 2)
    b <- rep(rnorm(N), each = 2)
    d <- rnorm(N * 2)

    # fixed effects
    beta <- list(
      us_valence = c(1, -1)
      , D = 1
      , C = 1
      , d = 1
    )

    # regression function:
    rating <-
      beta$D * D + # * beta$us_valence[us_valence] + # = only main effect
      beta$C * C * beta$us_valence[us_valence] + # = interaction w/ us valence
      beta$d * d * beta$us_valence[us_valence] + # = interaction w/ us valence
      beta$us_valence[us_valence] + # main effect us valence
      rnorm(30)[id] + # participant effects
      rnorm(60) # residuals
    beta <- NULL
  })


View(data)

library(lme4)
library(ggeffects)

contrasts(data$us_valence) <- "contr.sum"                   # = effects coding (i.e. sum-to-zero contrasts)
cntr <- center <- function(x) { x - mean(x, na.rm = TRUE) } # = function for centering

model <- lmer(
  formula = rating ~ (cntr(a) + cntr(b) + cntr(d) + cntr(D) + cntr(C)) * us_valence + (1 | id)
  , data = data
)
summary(model)

ggpredict(model, terms = ~ C * d * us_valence) |>
  plot(rawdata = TRUE)
