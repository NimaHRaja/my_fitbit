##### DEPRECATED — November 2026
#####
##### This script is part of the original Fitbit API workflow, using fitbitr.
##### Following the discontinuation of the Fitbit API, the workflow is being
##### migrated to Google Health using the ghealth CLI.
#####
##### Retained for reference but no longer maintained.
################################################################################

##### Gets the first $limit$ tracks from $start_date$ from fitbit and writes them into a local file.

get_and_save_list_of_acitivities <- function(start_date, limit){
  GET(
    paste(
      "https://api.fitbit.com/1.1/user/-/activities/list.json?afterDate=",
      start_date,
      "&sort=asc&offset=0&limit=",
      limit,
      sep=""),
    add_headers(
      .headers = c(
        Authorization = paste0("Bearer ", .fitbitr_token$credentials$access_token)
      ))) %>% 
    
    content("raw") %>% 
    writeBin(
      paste("data/activities_list/activities_list_",
            start_date,
            "_",
            limit,
            sep = ""))
}