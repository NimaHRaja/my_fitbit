##### Downloads TCX files for all activities from Google Health.
##### Stores tracks in monthly folders, with date preceding activity ID.
##### Skips previously downloaded tracks and attempts all activities,
##### regardless of GPS availability.

get_tracks <- function(){
  
  list_of_activities <-
    read.csv("data/clean_list_of_activities_ghealth.csv",
             colClasses = "character")
  
  for(i in seq_len(nrow(list_of_activities))){
    
    activity_id <- list_of_activities[i, "id"]
    activity_date <- substr(list_of_activities[i, "start"], 1, 10)
    
    output_dir <-
      paste0("data/tracks/ghealth/",
             substr(activity_date, 1, 7), "/")
    
    output_file <-
      paste0(output_dir, "track_", activity_date, "_",
             activity_id, ".tcx")
    
    if(!file.exists(output_file)){
      
      dir.create(output_dir, recursive = TRUE, showWarnings = FALSE)
      
      message("Downloading ", activity_id)
      
      results <-
        system2(
          ghealth,
          args = c(
            "data", "exercise", "export-tcx",
            "--id", activity_id,
            "--output", output_file
          ),
          stdout = TRUE,
          stderr = TRUE
        )
      
      if(!is.null(attr(results, "status"))){
        
        if(file.exists(output_file))
          file.remove(output_file)
        
        warning(paste("Failed to download", activity_id,
                      paste(results, collapse = "\n")))
        
      }
      
    }else{message(paste(i, "exists"))}
    
  }
  
}