best_ndays_minsteps <- 
  do.call("rbind",
          lapply(seq(from = 1, to = 500, by = 1), function(num_days){
            
            DF_rolling <- 
              DF_steps %>% 
              # filter(date < '2025-07-21') %>% 
              mutate(rolling_min = -rollmax(-steps, num_days, align = "right", fill = "NA"))
            
            
            
            DF_rolling %>% 
              filter(!is.na(rolling_min)) %>% 
              filter(rolling_min == max(rolling_min)) %>% 
              mutate(num_days = num_days) %>% 
              rename(min_steps = rolling_min) %>% 
              select(date, min_steps, num_days)
            
          }))


best_ndays_minsteps <- best_ndays_minsteps %>% 
  group_by(min_steps, num_days) %>% summarise(date = max(date), .groups = "drop")


best_ndays_minsteps_before <- 
  do.call("rbind",
          lapply(seq(from = 1, to = 500, by = 1), function(num_days){
            
            DF_rolling <- 
              DF_steps %>% 
              filter(date < '2025-04-22') %>%
              mutate(rolling_min = -rollmax(-steps, num_days, align = "right", fill = "NA"))
            
            
            
            DF_rolling %>% 
              filter(!is.na(rolling_min)) %>% 
              filter(rolling_min == max(rolling_min)) %>% 
              mutate(num_days = num_days) %>% 
              rename(min_steps = rolling_min) %>% 
              select(date, min_steps, num_days)
            
          }))


best_ndays_minsteps_before <- best_ndays_minsteps_before %>% 
  group_by(min_steps, num_days) %>% summarise(date = max(date), .groups = "drop")



rbind(best_ndays_minsteps, best_ndays_minsteps_before) %>%   
  filter(min_steps < 15000) %>%
  filter(min_steps > 5000) %>%
  ggplot(aes(x = num_days, y = min_steps, colour = substr(date,0,4))) + 
  geom_point()
