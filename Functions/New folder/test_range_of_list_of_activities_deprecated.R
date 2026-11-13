##### DEPRECATED — November 2026
#####
##### This script is part of the original Fitbit API workflow, using fitbitr.
##### Following the discontinuation of the Fitbit API, the workflow is being
##### migrated to Google Health using the ghealth CLI.
#####
##### Retained for reference but no longer maintained.
################################################################################

##### reads activities files and tests their sanity

a_file <- list.files("data/activities_list/")[3]

do.call("rbind",
        lapply(list.files("data/activities_list/"), function(x){
          activities_json <-
            read_json(paste("data/activities_list/", x, sep = ""))
          
          
          lapply(activities_json$activities, function(x) x$originalStartTime) %>% unlist() %>% as.data.frame() %>% 
            summarise(mn = min(.), mx = max(.))
        }
        )
) %>% 
  arrange(mn) %>% 
  mutate(mn <= lag(mx)) %>% 
  View()
