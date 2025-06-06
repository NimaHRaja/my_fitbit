##### Reads all activities_list (in JSON format), extracts the links to tcx files for individual tracks, 
##### removes the duplicates, and writes them into clean_list_of_activities.csv

all_list_of_activity_files <- list.files("data/activities_list/")

for (a_file in all_list_of_activity_files){   
  activities_json <-
    read_json(paste("data/activities_list/", a_file, sep = ""))
  
  do.call("rbind",
          lapply(activities_json$activities, function(x)
            data.frame(logId = x$logId %>% as.character(),
                       startTime = x$originalStartTime %>% as.character(),
                       tcxLink = ifelse(("tcxLink" %in% names(x)), x$tcxLink %>% as.character(), "")))) %>%
    write.table("data/clean_list_of_activities.csv", append = TRUE, col.names = FALSE, row.names = FALSE, sep = ",")
}


read.csv("data/clean_list_of_activities.csv", colClasses = rep("character",3)) %>% unique() %>% 
  arrange(startTime) %>% 
  filter(tcxLink != "") %>% 
  write.csv("data/clean_list_of_activities.csv", row.names = FALSE)



