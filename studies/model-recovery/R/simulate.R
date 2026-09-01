

simulate_participant <- function(
  D
  , C 
  , d
  , a
  , b
  , G = 1/4
  , n_cs
  , n_dist
  , attitudes = list(mean = 0, sd = 0)
  , attitude_based_guessing = FALSE
  , binary_guessing = FALSE
  , rating = c("8-point-likert", "binary-attitude-based", "binary-memory-dominant", "continuous")
  , sid
) {
  
  rating <- match.arg(rating, several.ok = FALSE)
  
  stimulus_type <- factor(
    c(rep("CS", n_cs),  rep("Distractor", n_dist))
  )

  cs_idx   <- stimulus_type == "CS"
  dist_idx <- stimulus_type == "Distractor"
  
  us_valence <- rep("new", n_cs + n_dist)
  us_valence[cs_idx] <- rep(c("positive", "negative"), length.out = n_cs)
  
  tau_attitude <- rnorm(n = n_cs + n_dist, mean = attitudes$mean, sd = attitudes$sd) * 
    ifelse(us_valence == "positive", 1, -1)
  
  
  discrimination <- rbinom(
    n = n_cs + n_dist
    , prob = D
    , size = 1
  )
  identity_memory <- valence_memory <- rep(0L, n_cs + n_dist)
  
  # CSs recognized as old...
  idx <- cs_idx & discrimination == 1L
  identity_memory[idx] <- rbinom(
    n = sum(idx)
    , prob = C
    , size = 1L
  )
  valence_memory[idx] <- rbinom(
    n = sum(idx)
    , prob = d
    , size = 1L
  )
  valence_memory[identity_memory == 1L] <- 1L
  
  # manifeste variablen
  old_new <- rep(NA, n_cs + n_dist)
  
  idx <- cs_idx & discrimination == 1L
  old_new[idx] <- "old"
  
  idx <- dist_idx & discrimination == 1L
  old_new[idx] <- "new"
  
  idx <- discrimination == 0L
  old_new[idx] <- sample(c("old", "new"), size = sum(idx), replace = TRUE, prob = c(b, 1-b)) # guessing old
  
  
  chosen_valence <- rep(NA, n_cs + n_dist)
  
  idx <- cs_idx & valence_memory == 1
  chosen_valence[idx] <- us_valence[idx]
  
  idx <- (cs_idx & valence_memory == 0 | dist_idx) & old_new == "old"

  if(attitude_based_guessing) {
    if(binary_guessing) {
      p_positive <- ifelse(tau_attitude[idx] > 0, 1, 0)
    } else {
      p_positive <- pnorm(tau_attitude[idx])
    }
  } else {
    p_positive <- a
  }
  
  chosen_valence[idx] <- as.character(factor(
    rbinom(n = sum(idx), size = 1, prob = p_positive)
    , levels = c(1, 0)
    , labels = c("positive", "negative")
  ))
  
  chosen_valence[old_new == "new"] <- ""
  
  correct_us <- rep(NA, n_cs + n_dist)
  
  # informed decision
  idx <- old_new == "old" & identity_memory == 1L
  correct_us[idx] <- "cor"
  
  # correct valence guessed
  idx <- old_new == "old" & identity_memory == 0 & chosen_valence == us_valence
  correct_us[idx] <- sample(c("cor", "incor"), prob = c(G, 1-G), size = sum(idx), replace = TRUE)
  
  # wrong valence guessed -> wrong US chosen
  idx <- identity_memory == 0L & old_new == "old" & chosen_valence != us_valence
  correct_us[idx] <- "incor"
  
  
  correct_us[old_new == "new"] <- "new"
  
  # evaluative ratings
  memory_influence <- rep(0, n_cs + n_dist)
  idx <- valence_memory == 1L
  
  memory_influence[idx] <- rnorm(n = sum(idx), mean = 1, sd = 1) * ifelse(us_valence[idx] == "positive", 1, -1)
  
  # evaluative_rating <- 1 + rbinom(n = n_cs + n_dist, size = 7, prob = pnorm(tau_attitude + memory_influence))
  if(rating == "binary-attitude-based") {
    evaluative_rating <- ifelse(tau_attitude > 0, "positive", "negative")
  } else if (rating == "binary-memory-dominant") {
    evaluative_rating <- rep(NA, n_cs + n_dist)
    idx <- valence_memory == 1L & sample(c(T, F), size = length(valence_memory), replace = TRUE)
    evaluative_rating[ idx] <- as.character(us_valence[idx])
    evaluative_rating[!idx] <- ifelse(tau_attitude[!idx] > 0, "positive", "negative")
  } else if(rating == "8-point-likert") {
    evaluative_rating <- round(pnorm(tau_attitude + memory_influence) * 7 + 1)
  } else {
    stop("rating not properly defined")
  }

  
  
  list2DF(list(
    stimulus_type = stimulus_type
    , us_valence = us_valence
    
    , tau_discrimination = discrimination
    , old_new = old_new
    
    , tau_valence_memory = valence_memory
    , chosen_valence = chosen_valence
    
    , tau_identity_memory = identity_memory
    , correct_us = correct_us
    , mpt_response = paste0(
      factor(us_valence, levels = c("positive", "negative", "new"), labels = c("PosUS", "NegUS", "New"))
      , factor(chosen_valence, levels = c("positive", "negative", ""), labels = c("pos", "neg", ""))
      , correct_us
    )
    , evaluative_rating = evaluative_rating
    , sid = rep(sid, length(stimulus_type))
  ))
}



d <- simulate_participant(D = .6, C = 0, d = 0, a = .5, b = .5, G = 1/4, n_cs = 1e5, n_dist = 1e5, attitude_based_guessing = TRUE, attitudes = list(mean = 1, sd = 2), sid = 1)
aggregate(
  cbind(
    p_old = old_new == "old"
    , correct_valence = chosen_valence == us_valence
    , correct_us = correct_us == "cor"
  ) ~ stimulus_type, data = d, FUN = mean
)

# library(HMMTreeC)

model_file <- file.path(rprojroot::find_rstudio_root_file(), "model-equations", "wsw-6.eqn")
response_levels <- MPTinR::check.mpt(model_file)$eqn.order.categories

d$mpt_response <- factor(d$mpt_response, levels = response_levels)

HMMTreeC::fit_mpt(
  model = model_file
  , data = as.data.frame(unclass(table(d$sid, d$mpt_response)))
  , restrictions = list(G = 1/4)
)

simulate_experiment <- function(
  n_subjs
  , n_cs = 24
  , n_dist = 24
  , D = .6
  , C = .4
  , d = .2
  , G = 1/4
  , a = .5
  , b = .5
  , attitude_based_guessing = FALSE
  , attitudes = list(mean = 0, sd = 1)
  , binary_guessing = FALSE
  , rating = c("8-point-likert", "binary-attitude-based", "binary-memory-dominant", "continuous")
) {
  
  rating <- match.arg(rating, several.ok = FALSE)
  
  eta <- list(
    D   = D
    , C = C
    , d = d
    , G = G
    , a = a
    , b = b
  ) |>
    lapply(qnorm) |>
    list2DF()

  sigma <- list2DF(list(
    D   = .5
    , C = .5
    , d = .5
    , G = 0
    , a = .2
    , b = .2
  ))
  
  theta <- Map(
    n = rep(n_subjs, ncol(eta))
    , mean = eta
    , sd = sigma
    , f = rnorm
  ) |>
    lapply(pnorm) |>
    setNames(nm = names(sigma)) |>
    list2DF()
  
  Map(
    f = simulate_participant
    , D = theta$D
    , C = theta$C
    , d = theta$d
    , G = theta$G
    , a = theta$a
    , b = theta$b
    , n_cs = n_cs
    , n_dist = n_dist
    , sid = seq_along(theta$D)
    , attitude_based_guessing = attitude_based_guessing
    , attitudes = rep(list(attitudes), n_subjs)
    , binary_guessing = binary_guessing
    , rating = rating
  ) |>
    do.call(what = "rbind")
}

# Simulation 3 ----
# 
# Does consistent responding between assignment task and evaluative ratings inflate d?
# For instance, pre-existing attitudes...

d <- simulate_experiment(
  n_subjs = 60
  , attitude_based_guessing = TRUE
  , attitudes = list(mean = 0, sd = 1)
)

d$mpt_response <- factor(d$mpt_response, levels = response_levels)
d$evaluative_rating

mpt_data <- as.data.frame(unclass(table(d$sid, d$mpt_response)))

hier_model <- TreeStan::fit_mpt(
  model = model_file
  , data = mpt_data
  , restrictions = list(G = 1/4)
  , parameterization = "latent_location"
)
summary(hier_model)

# Simulation 4 ----
# 
# Does consistent responding between assignment task and evaluative ratings inflate d if
# responses are based on conditioned attitudes?

d <- simulate_experiment(
  n_subjs = 60
  , attitude_based_guessing = TRUE
  , attitudes = list(mean = 1, sd = 1)
)

d$mpt_response <- factor(d$mpt_response, levels = response_levels)
d$evaluative_rating

mpt_data <- as.data.frame(unclass(table(d$sid, d$mpt_response)))

library(TreeStan)
sim4_model <- TreeStan::fit_mpt(
  model = model_file
  , data = mpt_data
  , restrictions = list(G = 1/4)
  , parameterization = "latent_location"
)
summary(sim4_model)


# Simulation 5 ----
# 
# Does consistent responding between assignment task and evaluative ratings inflate d if
# responses are based on conditioned attitudes?
# Does controlling for different a come to the rescue?

d <- simulate_experiment(
  n_subjs = 240
  , C = .1
  , d = 0.3
  , attitude_based_guessing = TRUE
  , attitudes = list(mean = 1, sd = 1)
)
d$mpt_response_2 <- paste0(d$evaluative_rating, "_", d$mpt_response)

response_levels <- MPTinR::check.mpt(file.path(rprojroot::find_rstudio_root_file(), "model-equations/wsw-6-wide.eqn"))$eqn.order.categories

d$mpt_response_2 <- factor(d$mpt_response_2, levels = response_levels)

mpt_data <- as.data.frame(unclass(table(d$sid, d$mpt_response_2)))

library(TreeStan)
sim5_model <- TreeStan::fit_mpt(
  model = file.path(rprojroot::find_rstudio_root_file(), "model-equations/wsw-6-wide.eqn")
  , data = mpt_data
  , restrictions = list(G = 1/4)
  , parameterization = "latent_location"
  , refresh = 50
)
summary(sim5_model)

# Simulation 6 ----
# What if both memory responses and evaluative ratings flow directly from true conditioned attitudes?

TreeBUGS::withinSubjectEQN(
  "model-equations/wsw-6.eqn"
  , save = "model-equations/wsw-6-binary-ratings.eqn"
  , constant = c("D", "C", "d", "G", "b")
  , labels = c("positive", "negative")
)
d <- simulate_experiment(
  n_subjs = 60
  , C = .2
  , d = 0.3
  , attitude_based_guessing = TRUE
  , attitudes = list(mean = 1, sd = 1)
  , rating = "binary-attitude-based"
  , binary_guessing = TRUE
)
d$mpt_response_2 <- paste0(d$evaluative_rating, "_", d$mpt_response)

response_levels <- MPTinR::check.mpt(file.path(rprojroot::find_rstudio_root_file(), "model-equations/wsw-6-binary-ratings.eqn"))$eqn.order.categories

d$mpt_response_2 <- factor(d$mpt_response_2, levels = response_levels)

mpt_data <- as.data.frame(unclass(table(d$sid, d$mpt_response_2)))

library(TreeStan)
sim6_model <- TreeStan::fit_mpt(
  model = file.path(rprojroot::find_rstudio_root_file(), "model-equations/wsw-6-binary-ratings.eqn")
  , data = mpt_data
  , restrictions = list(G = 1/4) # , a = c("a_positive", "a_negative"))
  , parameterization = "latent_location"
  , refresh = 50
)
summary(sim6_model)

# Simulation 7 ----
d <- simulate_experiment(
  n_subjs = 60
  , C = .2
  , d = 0.3
  , attitude_based_guessing = TRUE
  , attitudes = list(mean = 1, sd = 1)
  , rating = "binary-memory-dominant"
  , binary_guessing = TRUE
)
d$mpt_response_2 <- paste0(d$evaluative_rating, "_", d$mpt_response)

response_levels <- MPTinR::check.mpt(file.path(rprojroot::find_rstudio_root_file(), "model-equations/wsw-6-binary-ratings.eqn"))$eqn.order.categories

d$mpt_response_2 <- factor(d$mpt_response_2, levels = response_levels)

mpt_data <- as.data.frame(unclass(table(d$sid, d$mpt_response_2)))


sim7_model <- TreeStan::fit_mpt(
  model = file.path(rprojroot::find_rstudio_root_file(), "model-equations/wsw-6-binary-ratings.eqn")
  , data = mpt_data
  , restrictions = list(G = 1/4)
  , parameterization = "latent_location"
  , refresh = 50
)
summary(sim7_model)


# Simulation 8 ----
d <- simulate_experiment(
  n_subjs = 60
  , C = .2
  , d = 0.3
  , attitude_based_guessing = TRUE
  , attitudes = list(mean = 0, sd = 1)
  , rating = "binary-memory-dominant"
  , binary_guessing = TRUE
)
d$mpt_response_2 <- paste0(d$evaluative_rating, "_", d$mpt_response)

response_levels <- MPTinR::check.mpt(file.path(rprojroot::find_rstudio_root_file(), "model-equations/wsw-6-binary-ratings.eqn"))$eqn.order.categories

d$mpt_response_2 <- factor(d$mpt_response_2, levels = response_levels)

mpt_data <- as.data.frame(unclass(table(d$sid, d$mpt_response_2)))


sim8_model <- TreeStan::fit_mpt(
  model = file.path(rprojroot::find_rstudio_root_file(), "model-equations/wsw-6-binary-ratings.eqn")
  , data = mpt_data
  , restrictions = list(G = 1/4)
  , parameterization = "latent_location"
  , refresh = 50
)
summary(sim8_model)

