

cntr <- center <- function(x, na.rm = TRUE) { x - mean(x, na.rm = na.rm) }


simulate_1 <- function(n = 20) {

  n_posCS <- 5
  n_negCS <- 5
  n_USperCS <- 5
  idmem_proportions <- c(0, 0 ,0,.2,.4,.6,.8)

  
  y <- data.frame(
    sid = rep(seq_len(n), each = n_posCS + n_negCS)
    , cs_identity = unlist(replicate(n = n, sample(factor(paste0("cs_",1:10))), simplify = FALSE))
    , us_valence  = factor(rep(1:2, each = 5, times = n), levels = 1:2, labels = c("positive", "negative"))
  )

  stopifnot(all(with(y, table(sid, cs_identity)) == 1L))
  y$idmem_prop <- sample(idmem_proportions, size = nrow(y), replace = TRUE)

  # US valence memory
  y$valmem_correct <- NA
  y$valmem_correct[y$idmem_prop > 0] <- 1 # rbinom(n = sum(y$idmem_prop == 0), size = 1, prob = .5 + y$id_mem_prop/ 2)
  y$valmem_correct[y$idmem_prop == 0] <- rbinom(n = sum(y$idmem_prop == 0), size = 1, prob = .5)
  
  # US identity memory
  y$idmem_1_correct <- rbinom(n = nrow(y), size = 1, prob = y$idmem_prop)
  y$idmem_2_correct <- rbinom(n = nrow(y), size = 1, prob = y$idmem_prop)
  y$idmem_3_correct <- rbinom(n = nrow(y), size = 1, prob = y$idmem_prop)
  y$idmem_4_correct <- rbinom(n = nrow(y), size = 1, prob = y$idmem_prop)
  y$idmem_5_correct <- rbinom(n = nrow(y), size = 1, prob = y$idmem_prop)
  
  # Ratings ----
  y$evaluative_rating <- NA
  
  idx <- y$idmem_prop > 0 & y$us_valence == "positive"
  y$evaluative_rating[idx] <- rnorm(n = sum(idx), mean =  1, sd = 1)
  idx <- y$idmem_prop > 0 & y$us_valence == "negative"
  y$evaluative_rating[idx] <- rnorm(n = sum(idx), mean = -1, sd = 1)
  idx <- y$idmem_prop == 0
  y$evaluative_rating[idx] <- rnorm(n = sum(idx), mean =  0, sd = 1)
  
  # return
  y
}
  

data_list <- lapply(seq_len(1e3), simulate_1)

analyze <- function(x) {
  x$idmem_mean <- (x$idmem_1_correct + x$idmem_2_correct + x$idmem_3_correct + x$idmem_4_correct + x$idmem_5_correct) / 5
  x$ec <- ifelse(x$us_valence == "positive", x$evaluative_rating, -x$evaluative_rating)
  
  
  # 'Initial analysis'
  agg <- aggregate(cbind(avg_ec = ec) ~ sid, data = x, FUN = mean)
  
  x <- merge(x, agg)
  
  res_model <- lm(ec ~ avg_ec, data = x)
  x$init_residuals <- unname(residuals(res_model))
  
  # Analysis proper
  summary(lm(
    formula = init_residuals ~ valmem_correct + idmem_mean
    , data = x
  ))$coefficients
}

res <- lapply(data_list, analyze)

lm_res <- lapply(res, function(x) {
  x <- as.data.frame(x)
  x$term <- rownames(x)
  rownames(x) <- NULL
  x
}) |> do.call(what = "rbind")

par(mfrow = c(3, 2))
out <- split(lm_res, lm_res$term) |>
  lapply(with, {
    hist(Estimate, freq = F, sub = term[[1]])
    hist(`Pr(>|t|)`, freq = FALSE, breaks = seq(0, 1, .05), sub = term[[1]])
  })

