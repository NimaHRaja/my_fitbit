top2_ndays_minsteps <- 
  do.call("rbind",
          lapply(seq(from = 1, to = 500, by = 1), function(num_days){
            
            DF_steps %>% 
              mutate(rolling_min = -rollmax(-steps, num_days, align = "right", fill = "NA")) %>% 
              mutate(num_days = num_days) %>% 
              filter(!is.na(rolling_min)) %>% 
              group_by(rolling_min) %>% 
              filter(date == max(date)) %>% 
              ungroup() %>% 
              group_by(num_days) %>% 
              mutate(rank = rank(-rolling_min)) %>% 
              ungroup() %>% 
              filter(rank <= 5)
            # filter(rank <= 2)
          }))


top2_ndays_minsteps %>% 
  filter(rolling_min < 15000) %>% 
  filter(num_days <= 200) %>% 
  ggplot(aes(x = num_days, y = rolling_min, colour = substr(date,0,4))) +
  geom_point()



top2_ndays_minsteps %>% 
  filter(rolling_min < 15000) %>% 
  filter(num_days <= 200) %>% 
  ggplot(aes(x = num_days, y = rolling_min, colour = substr(date,0,4))) +
  geom_point() +
  facet_wrap(. ~ rank)

top2_ndays_minsteps %>% 
  filter(rolling_min < 15000) %>% 
  filter(num_days <= 200) %>% 
  mutate(dd = substr(date,0,4)) %>% 
  ggplot(aes(x = num_days, y = rolling_min, colour = as.factor(rank))) +
  geom_point() +
  facet_wrap(. ~ dd)



top2_ndays_minsteps %>% 
  filter(rolling_min < 15000) %>%
  filter(num_days <= 150) %>% 
  # filter(num_days >= 20) %>% 
  filter(rank <= 3) %>% 
  mutate(dd = substr(date,0,4)) %>% 
  ggplot(aes(x = num_days, y = rolling_min, colour = as.factor(rank))) +
  geom_point() +
  facet_grid(rank ~ dd)


top2_ndays_minsteps %>% 
  # filter(rolling_min < 15000) %>% 
  # filter(num_days <= 200) %>% 
  ggplot(aes(x = num_days, y = date, colour = as.factor(rank))) +
  geom_point()


top2_ndays_minsteps %>% 
  # filter(rolling_min < 15000) %>% 
  # filter(num_days <= 200) %>% 
  ggplot(aes(x = num_days, y = date, colour = as.factor(rank))) +
  geom_point() +
  facet_wrap(. ~ rank)


 top2_ndays_minsteps %>% 
  mutate(gg = paste(date,rolling_min,sep = "--")) %>% 
  dcast(num_days ~ rank, value.var = "gg", fun.aggregate = min) %>% 
  View()





top2_ndays_minsteps %>% 
  mutate(rank = paste("r",rank,sep = "")) %>%
  mutate(gg = paste(rolling_min,sep = "--")) %>%  
  dcast(num_days ~ rank, value.var = "gg", fun.aggregate = min) %>% 
  group_by(r1,r2,r3,r4,r5) %>% 
  summarise(max(num_days)) %>% View()
