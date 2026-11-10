# for (a_file in all_list_of_activity_files){   
#   activities_json <-
#     read_json(paste("data/activities_list/", a_file, sep = ""))

all_list_of_activity_files <- list.files("data/activities_list/")
a_file <- all_list_of_activity_files[199]

activities_json <-
  read_json(paste("data/activities_list/", a_file, sep = ""))



do.call("rbind",
        lapply(activities_json$activities, function(x)
          if(x$activityName %in% c("Run", "Walk")){
            data.frame(
              start = gsub("Z", "",gsub("T", " ",x$originalStartTime)) %>% as.POSIXct(),
              end = gsub("Z", "",gsub("T", " ",x$originalStartTime)) %>% as.POSIXct() +  x$originalDuration/1000,
              type = x$activityName,
              steps = x$steps,
              logType = x$logType
            )
          } else {
            data.frame(
              start = gsub("Z", "",gsub("T", " ",x$originalStartTime)) %>% as.POSIXct(),
              end = gsub("Z", "",gsub("T", " ",x$originalStartTime)) %>% as.POSIXct() +  x$originalDuration/1000,
              type = x$activityName,
              steps = 0,
              logType = x$logType
            )
          }
        )) %>% View()




do.call("rbind",
        lapply(activities_json$activities, function(x)
          if(x$activityName %in% c("Run", "Walk")){
            data.frame(
              start = gsub("Z", "",gsub("T", " ",x$originalStartTime)) %>% as.POSIXct(),
              end = gsub("Z", "",gsub("T", " ",x$originalStartTime)) %>% as.POSIXct() +  x$originalDuration/1000,
              type = x$activityName,
              steps = x$steps,
              logType = x$logType
            )
          } else {
            data.frame(
              start = gsub("Z", "",gsub("T", " ",x$originalStartTime)) %>% as.POSIXct(),
              end = gsub("Z", "",gsub("T", " ",x$originalStartTime)) %>% as.POSIXct() +  x$originalDuration/1000,
              type = x$activityName,
              steps = 0,
              logType = x$logType
            )
          }
        )) %>% 
  mutate(date = substr(start, 0, 10)) %>% 
  filter(type == "Run") %>% 
  group_by(date) %>% 
  summarise(steps = sum(steps))



do.call("rbind",
        lapply(activities_json$activities, function(x)
            data.frame(
              aa = x$originalStartTime %>% substr(0,10),
              aaa = x$originalStartTime %>% substr(12,16),
              bb = (gsub("Z", "",gsub("T", " ",x$originalStartTime)) %>% as.POSIXct() +  x$originalDuration/1000) %>% substr(0,10),
              bb = (gsub("Z", "",gsub("T", " ",x$originalStartTime)) %>% as.POSIXct() +  x$originalDuration/1000) %>% substr(12,16),
              type = x$activityName,
              logType = x$logType,
              ss = x$start
            )
        )) %>% View()




