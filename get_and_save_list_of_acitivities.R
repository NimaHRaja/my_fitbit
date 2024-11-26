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