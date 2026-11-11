##### Loading libraries and connecting to Google Health

library(dplyr)
library(jsonlite)

ghealth <- file.path(
  Sys.getenv("USERPROFILE"),
  "google-health-cli",
  "ghealth.exe"
)

stopifnot(file.exists(ghealth))