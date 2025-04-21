
temporary_file <- tempfile(fileext = ".R")
download.file(
  url = "https://gist.githubusercontent.com/mariusbarth/5532e908397a7c84d9d99b307d0ea6e9/raw/depends.R"
  , destfile = temporary_file
)
source(temporary_file, echo = FALSE)
