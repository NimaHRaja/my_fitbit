##### Checks that consecutive batches of activity lists overlap.
##### Stops execution if a potential gap is detected.

test_range_of_list_of_activities <- function(){
  
  all_files <-
    list.files("data/activities_list/ghealth/",
               pattern = "\\.json$",
               full.names = TRUE)
  
  ranges <-
    do.call("rbind",
            lapply(all_files, function(a_file){
              
              activities_json <- read_json(a_file)
              
              start_times <-
                unlist(lapply(activities_json$dataPoints, function(x) x$start))
              
              data.frame(
                file_date = substr(basename(a_file), 17, 26),
                mn = min(start_times),
                mx = max(start_times)
              )
              
            })) %>%
    group_by(file_date) %>%
    summarise(mn = min(mn), mx = max(mx), .groups = "drop") %>%
    arrange(file_date) %>%
    mutate(overlap = mx >= lead(mn))
  
  print(ranges)
  
  if(any(ranges$overlap == FALSE, na.rm = TRUE))
    stop("Potential gap detected between activity lists.")
  
  message("Activity list ranges validated.")
  
}