DF_steps  %>%
  mutate(num_days = (as.Date('2026-04-29')- date) %>% as.numeric() + 1) %>% 
  mutate(a = 39309603 - a) %>%
  inner_join(best_ndays, by = "num_days") %>%
  mutate(gg = mean_steps * num_days - a) %>%
  # filter(date.y < '2025-11-11') %>%
  # filter(gg < 14000) %>%
  View()

DF_steps  %>%
  mutate(num_days = (as.Date('2026-04-29')- date) %>% as.numeric() + 1) %>% 
  mutate(a = 39309603 - a) %>%
  inner_join(best_ndays, by = "num_days") %>%
  mutate(gg = mean_steps * num_days - a) %>%
  filter(gg < 17000) %>%
  filter(date.y < '2025-11-11') %>%
  ggplot(aes(x = num_days, y = gg, colour = substr(date.y,0,4))) +
  geom_point()



do.call("rbind",
        lapply(seq(from = 1, to = 1000, by = 1), function(num_days){
          
          DF_steps %>%
            filter( (as.Date('2026-04-29')- date) < num_days) %>% 
            summarise(target_steps = min(steps)) %>% 
            mutate(num_days = num_days + 1)
        }
        )) %>% View()
  ggplot(aes(x = num_days, y = target_steps)) +
  geom_point(colour = "blue")




do.call("rbind",
        lapply(seq(from = 1, to = 500, by = 1), function(num_days){
          
          DF_steps %>%
            filter( (as.Date('2026-04-29')- date) < num_days) %>% 
            summarise(target_steps = min(steps)) %>% 
            mutate(num_days = num_days + 1)
        }
        )) %>% 
  inner_join(best_ndays_minsteps, by = "num_days") %>% 
  filter(target_steps > min_steps) %>%
  # filter(date < '2025-04-12') %>%
  View()


do.call("rbind",
        lapply(seq(from = 1, to = 500, by = 1), function(num_days){
          
          DF_steps %>%
            filter( (as.Date('2026-04-29')- date) < num_days) %>% 
            summarise(target_steps = min(steps)) %>% 
            mutate(num_days = num_days)
        }
        )) %>% 
  filter(target_steps > 0) %>% 
  group_by(target_steps) %>%
  summarise(max(num_days)) %>%
  View()





num_days <- 28
cross_join(
  DF_steps %>% select(date),
  DF_steps %>% select(-a) %>% rename(date2 = date)) %>% 
  filter(((date2-date) %>% as.integer()) <= num_days) %>% 
  filter(((date2-date) %>% as.integer()) >= 0) %>% 
  group_by(date) %>% 
  summarise(window_min_steps = min(steps)) %>% 
  cross_join(
    DF_steps %>% select(date) %>% rename(date2 = date)) %>% 
  filter(date2 > date) %>% 
  group_by(date2) %>% 
  mutate(num_steps = max(window_min_steps)) %>% 
  filter(num_steps == window_min_steps) %>% 
  group_by(date2, num_steps) %>% 
  summarise(date = max(date)) %>% 
  ggplot(aes(x = date2, y = num_steps,colour = substr(date,0,4))) +
  geom_point()
 
