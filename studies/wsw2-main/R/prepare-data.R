
library(dplyr)
library(tidyr)
library(tinylabels)

study_folder <- file.path(rprojroot::find_rstudio_root_file(), "studies", "wsw2-main")

data <- file.path(study_folder, "data-raw", c("wsw2_rawdata.txt")) |>
  lapply(function(x) {
    readLines(x) |>
    lapply(FUN = `[[`, i = 1L) |>
    lapply(FUN = Filter, f = function(x) x != "") |>
    lapply(jsonlite::fromJSON, flatten = TRUE) |>
    # lapply(function(x) {x$uss <- NULL; x}) |>
    dplyr::bind_rows() # unequal number of columns!
    # do.call(what = "rbind")
  }) |>
  dplyr::bind_rows()

# data <- readRDS(file.path(study_folder, "data/study2.rds"))

data <- data |>
  fill(url.srid, .direction = "down") |>
  group_by(url.srid) |>
  fill(consent, pay_attention, serious, order, .direction = "downup")

# data$sports <- ifelse(
  # data$'-1'==TRUE | data$'-2'==TRUE | data$'-3'==TRUE | data$'-4'==TRUE | data$'-5'==TRUE | data$'-0'==TRUE | data$'-6'==TRUE
  # , 0
  # , 1
# )


data <- within(data, {
  cs_rating  <- as.numeric(cs_rating)
  url.srid   <- factor(url.srid)
  us_valence <- factor(us_valence, levels = c("positive", "negative"))
  reco_resp  <- factor(reco_resp, levels = c("new", "old"))
  evaluative_rating <- as.numeric(cs_rating)
  sid <- as.integer(url.srid)
  task_order <- factor(order, levels = c("eval_first", "mem_first"), labels = c("Rating first", "Memory first"))
  pay_attention <- as.integer(pay_attention)
  serious <- as.integer(serious)
  idx_us <- as.integer(gsub(source_mem, pattern = "^us", replacement = ""))
})

# Exclusion criteria ----
# always the same evaluative rating
rating_sd <- aggregate(evaluative_rating ~ sid, data = data, FUN = sd)
# always old or always new
prop_old <- aggregate(cbind(prop_old = as.numeric(reco_resp) - 1) ~ sid, data = data, FUN = mean)
# always the same US position
us_position <- aggregate(idx_us ~ sid, data = data, FUN = sd)

test_runs <- integer(0L)

excluded_participants <- list(
  not_attention = subset(data, !duplicated(sid) & pay_attention == 0)$sid
  , not_serious = subset(data, !duplicated(sid) & serious == 0)$sid
  , test_runs   = test_runs
  , always_gave_the_same_rating = with(rating_sd, sid[evaluative_rating == 0])
  , proportion_old              = with(prop_old , sid[prop_old == 0 | prop_old == 1])
  , always_the_same_us_position = with(us_position, sid[idx_us == 0])
)

data <- subset(
  data
  , # sports == 1 & 
    pay_attention == 1 & serious == 1 & !(url.srid %in% test_runs) & !(sid %in% unlist(excluded_participants))
  , select = c("sid","sender","consent","duration","ended_on","pay_attention","serious","response","response_action","comment_study","count_trial_learning","cs","us","us_valence","uss","count_trial_memory","idtarg","reco_resp","idx_us","count_trial_ratings","evaluative_rating", "task_order")
) |>
  label_variables(
    evaluative_rating = "Evaluative rating"
    , us_valence = "US valence"
    , task_order = "Task order"
  )

data_list <- split(data, f = data$sender)

cols_all <- c("sid", "task_order", "cs", "us", "us_valence")

recognition <- subset(data_list$recognition_trial, select = c(cols_all, "reco_resp"))
source      <- subset(data_list$source_trial     , select = c(cols_all, "uss", "idtarg", "idx_us"))
memory      <- merge(recognition, source, by = cols_all)


memory$correct_us_source <- memory$us
memory$source_mem_resp <- unlist(Map(x = memory$uss, i = memory$idx_us, f = `[`))

table(memory$us_valence, useNA = "ifany")

all_uss <- unique(memory$source_mem_resp) |>
  Filter(f = Negate(is.na))

USs <- split(
  all_uss
  , f = factor(as.integer(grepl(all_uss, pattern = "^p")), levels = c(1, 0), labels = c("positive", "negative"))
)

memory$mpt_response <- with(memory, {
  tree <- rep("New", length(us_valence))
  us_valence <- as.character(us_valence)
  us_valence[is.na(us_valence)] <- "dist"
  tree[us_valence == "positive"] <- "PosUS"
  tree[us_valence == "negative"] <- "NegUS"
  
  chosen_valence <- rep("neg", length(source_mem_resp))
  chosen_valence[source_mem_resp %in% USs$positive] <- "pos"
  
  correct_US <- ifelse(source_mem_resp==correct_us_source, "cor", "incor")
  
  mpt_response <- paste0(tree, "new")
  idx <- reco_resp == "old"
  mpt_response[idx] <- paste0(tree[idx], chosen_valence[idx], correct_US[idx])
  mpt_response
})
memory$mpt_response_condition <- paste0(
  memory$mpt_response
  , ifelse(memory$task_order == "Rating first", "_x1", "_x2")
)

unique(memory$mpt_response)
level_order <- MPTinR::check.mpt(file.path(study_folder, "WSW_exp2.eqn"))$eqn.order.categories

memory$mpt_response_condition <- factor(memory$mpt_response_condition, levels = level_order)
mpt_data <- unclass(table(memory$sid, memory$mpt_response_condition))
mpt_data_hierarchical <- as.data.frame(unclass(table(memory$sid, memory$mpt_response)))
mpt_data_hierarchical$sid <- as.integer(rownames(mpt_data_hierarchical))
mpt_data_hierarchical <- merge(
  mpt_data_hierarchical
  , subset(memory, !duplicated(sid), select = c("sid", "task_order"))
  , sort = FALSE
)
rownames(mpt_data_hierarchical) <- as.character(mpt_data_hierarchical$sid)

# Evaluative ratings ----
rating <- subset(
  data_list$rating_trial
  , select = c(cols_all, "evaluative_rating")
) |>
  droplevels()

rating_aggregated <- aggregate(evaluative_rating ~ us_valence + task_order + sid, data = rating, FUN = mean)
rating_wide <- pivot_wider(rating_aggregated, values_from = "evaluative_rating", names_from = "us_valence") |>
  within({
    ec_effect <- positive - negative
  })


dir.create(file.path(study_folder, "data"), showWarnings = FALSE)
saveRDS(
  list(
    rating = rating
    , rating_wide = rating_wide
    , memory = memory
    , mpt_data = mpt_data
    , mpt_data_hierarchical = mpt_data_hierarchical
    , excluded_participants = excluded_participants
  )
  , file = file.path(study_folder, "data", "data.rds")
)
