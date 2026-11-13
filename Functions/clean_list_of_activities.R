##### Reads all Google Health activity lists (JSON format),
##### extracts activity IDs and start times, removes duplicates,
##### and saves the results as a CSV file.

clean_list_of_activities <- function(){
  all_list_of_activity_files <-
    list.files("data/activities_list/ghealth/",
               pattern = "\\.json$",
               full.names = TRUE)
  
  list_of_activities <-
    do.call("rbind",
            lapply(all_list_of_activity_files, function(a_file){
              
              activities_json <- read_json(a_file)
              
              do.call("rbind",
                      lapply(activities_json$dataPoints, function(x)
                        data.frame(
                          id = as.character(x$id),
                          start = as.character(x$start),
                          end = as.character(x$end),
                          exerciseType = as.character(x$exerciseType),
                          hasGps = if(is.null(x$exerciseMetadata$hasGps)) NA else x$exerciseMetadata$hasGps,
                          source = as.character(x$source)
                        )))
            }))
  
  list_of_activities %>%
    distinct(id, .keep_all = TRUE) %>%
    arrange(start) %>%
    write.csv("data/clean_list_of_activities_ghealth.csv",
              row.names = FALSE)
}