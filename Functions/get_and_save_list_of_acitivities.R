##### Gets all activities from $start_date$ from Google Health.
##### Saves them as JSON files, with a maximum of 25 activities per file.
##### Automatically retrieves additional pages when necessary.

get_and_save_list_of_acitivities <- function(start_date){

  output_dir <- "data/activities_list/ghealth/"

  dir.create(output_dir, recursive = TRUE, showWarnings = FALSE)

  old_files <- list.files(
    output_dir,
    pattern = paste0("^activities_list_", start_date, "_[0-9]{3}\\.json$"),
    full.names = TRUE
  )

  page <- 1
  page_token <- NULL
  saved_files <- character()
  total <- 0

  repeat {

    output_file <-
      paste0(output_dir, "activities_list_",
             start_date, "_", sprintf("%03d", page), ".json")

    args <- c(
      "data", "exercise", "list",
      "--from", start_date,
      "--limit", "25",
      "--output", output_file
    )

    if(!is.null(page_token))
      args <- c(args, "--page-token", page_token)

    results <-
      system2(ghealth, args = args, stdout = TRUE, stderr = TRUE)

    if(!is.null(attr(results, "status")))
      stop(paste(results, collapse = "\n"))

    activities <- read_json(output_file)

    saved_files <- c(saved_files, output_file)
    total <- total + length(activities$dataPoints)

    message("Page ", page, ": ",
            length(activities$dataPoints), " activities saved.")

    page_token <- activities$nextPageToken

    if(is.null(page_token) || !nzchar(page_token))
      break

    page <- page + 1

  }

  # Remove obsolete pages from previous runs with the same start_date
  obsolete_files <- setdiff(old_files, saved_files)

  if(length(obsolete_files) > 0)
    file.remove(obsolete_files)

  message("Completed: ", total, " activities in ", page, " files.")

}