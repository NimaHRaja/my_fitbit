list_of_activities <- 
  read.csv("data/clean_list_of_activities.csv", colClasses = rep("character",3)) 

for(i in 1:dim(list_of_activities)[1]){
  
  output_file <- 
    paste("data/tracks/track_",
          list_of_activities[i,'logId'],
          "_",
          substr(list_of_activities[i,'startTime'], 0, 10),
          ".tcx",
          sep = "")
  
  if(!file.exists(output_file) & list_of_activities[i,'tcxLink'] != ""){
    
    print(paste("downloading", list_of_activities[i,'logId']))
    
    results <- 
      GET(
        list_of_activities[i,'tcxLink'],
        add_headers(
          .headers = c(
            Authorization = paste0("Bearer ", .fitbitr_token$credentials$access_token))))
    
    if(results$status_code == 200)
    {
      results %>% 
        content("raw") %>% 
        writeBin(output_file)
    }else{print("limit reached")}
    
  }else{print(paste(i, "exists"))}
}
