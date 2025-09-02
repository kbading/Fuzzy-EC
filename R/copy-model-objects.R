rds_files <- list.files(path = "studies", pattern = "treestan.rds$", recursive = TRUE, full.names = TRUE)
model_objects <- rds_files[grepl(rds_files, pattern = "model-objects")]

new_names <- paste0(
  "~/sciebo/most-amazing-woman-in-the-world/Fuzzy-EC/"
  , gsub(model_objects, pattern = "studies/|model-objects/", replacement = "")
)
file.copy(model_objects, new_names)
