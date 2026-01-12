library(dplyr)
library(fitbitr)
library(ggplot2)
library(zoo)

nima_login_data <- 
  read.csv("my_login_data.csv")

.fitbitr_token <- generate_fitbitr_token(
  app_name = nima_login_data$app_name,
  client_id = nima_login_data$client_id,
  client_secret = nima_login_data$client_secret,
  callback = nima_login_data$callback
)

DF_steps <- 
  rbind(
    get_steps("2024-01-01", "2025-11-30"),
    get_steps("2022-01-01", "2023-12-31"),
    get_steps("2020-01-01", "2021-12-31"),
    get_steps("2018-01-01", "2019-12-31"),
    get_steps("2016-01-01", "2017-12-31")
  )

DF_steps <- 
  DF_steps %>% 
  arrange(date) %>% 
  mutate(a = cumsum(steps))

best_ndays <- 
  do.call("rbind",
          lapply(1:1000, function(num_days){
            DF_rolling <- 
              DF_steps  %>% mutate(b = lag(n = num_days,a)) %>% mutate(mean_steps = (a - b)/num_days) %>% 
              mutate(mean_steps = if_else(is.na(mean_steps),a/num_days,mean_steps)) %>% select(-a,-b) %>% 
              mutate(num_days = num_days)
            
            DF_rolling[which.max(DF_rolling$mean_steps),]
          }))

best_ndays %>% 
  group_by(substr(date,0,4)) %>% 
  summarise(min(num_days), max(num_days))


best_ndays %>% 
  filter(mean_steps < 15000) %>% 
  ggplot(aes(x = num_days, y = mean_steps)) + 
  geom_point(colour = "blue")


best_ndays %>%
  filter(mean_steps < 15000) %>% 
  ggplot(aes(x = num_days, y = mean_steps, colour = substr(date,0,4))) + 
  geom_point()


best_ndays %>%
  mutate(date = if_else(date == max(date), date + 365, date)) %>%
  filter(mean_steps < 15000) %>% 
  ggplot(aes(x = num_days, y = mean_steps, colour = substr(date,0,4))) + 
  geom_point()


best_ndays %>% 
  filter(mean_steps < 15000) %>% 
  ggplot(aes(x = num_days, y = mean_steps, colour = substr(date,0,7))) + 
  geom_point()



# best_ndays %>%
#   filter(mean_steps < 15000) %>% 
#   ggplot(aes(x = num_days, y = mean_steps, colour = date)) + 
#   geom_point() +
#   scale_colour_gradient(low = "green", high = "red")


best_ndays %>% 
  ggplot(aes(x = num_days, y = date)) + 
  geom_point(colour = "blue") 


# best_ndays %>%
#   ggplot(aes(x = num_days, y = date, colour = mean_steps)) + 
#   geom_point() +
#   scale_colour_gradient(low = "green", high = "red")


best_ndays %>% group_by(date) %>% summarise(freq = n()) %>% arrange(desc(freq)) %>% View()


best_ndays %>% mutate(date = substr(date,0,7)) %>% group_by(date) %>% summarise(freq = n()) %>% 
  arrange(desc(freq)) %>% View()


best_ndays %>% mutate(date2 = date- num_days) %>% group_by(date2) %>% summarise(freq = n()) %>% 
  arrange(desc(freq)) %>% View()


best_ndays %>% mutate(date2 = substr(date- num_days,0,7)) %>% group_by(date2) %>% summarise(freq = n()) %>% 
  arrange(desc(freq)) %>% View()

##################

DF_steps %>% 
  filter(date > '2016-09-10') %>% 
  mutate(maxa = max(a)) %>% 
  mutate(today = max(date)) %>% 
  mutate(mean_steps_last_since_date = (maxa - a)/((as.Date(today) - as.Date(date)) %>% as.integer())) %>% 
  ggplot(aes(x = date, y = mean_steps_last_since_date)) + 
  geom_point(colour = "blue")



DF_steps %>% 
  mutate(b = lag(n = 7,a)) %>%
  mutate(mean_steps = (a - b)/7) %>% 
  mutate(mean_steps = if_else(is.na(mean_steps),a/7,mean_steps)) %>%
  filter(date > '2016-09-17') %>%
  ggplot(aes(x = date, y = mean_steps)) + 
  geom_point(colour = "blue") + 
  geom_smooth() +
  ggtitle("7-days moving average")



DF_steps %>% 
  mutate(b = lag(n = 28,a)) %>%
  mutate(mean_steps = (a - b)/28) %>% 
  mutate(mean_steps = if_else(is.na(mean_steps),a/28,mean_steps)) %>%
  filter(date > '2016-10-08') %>% 
  ggplot(aes(x = date, y = mean_steps)) + 
  geom_point(colour = "blue") + 
  geom_smooth() +
  ggtitle("28-days moving average")



DF_steps %>% 
  mutate(b = lag(n = 365,a)) %>%
  mutate(mean_steps = (a - b)/365) %>% 
  mutate(mean_steps = if_else(is.na(mean_steps),a/365,mean_steps)) %>% 
  filter(date > '2017-09-09') %>% 
  ggplot(aes(x = date, y = mean_steps)) + 
  geom_point(colour = "blue") + 
  geom_smooth() +
  ggtitle("365-days moving average")


DF_steps %>% 
  ggplot(aes(x = date, y = steps)) + 
  geom_smooth()


DF_steps %>% 
  filter(date >= '2021-01-01') %>% 
  ggplot(aes(x = date, y = steps)) + 
  geom_point(colour = "blue") +
  # geom_hline(yintercept = 10000, colour = "red") +
  # geom_hline(yintercept = 11111, colour = "red") +
  geom_hline(yintercept = 12000, colour = "red") +
  geom_hline(yintercept = 12500, colour = "red") +
  ylim(c(11000,14000))

#################

DF <- 
  do.call("rbind",
          lapply(seq(from = 7500, to = 15000, by = 10), function(target){
            
            DF_steps <- 
              DF_steps %>% 
              # filter(date < '2025-06-01') %>% 
              mutate(streak = 0) %>%
              mutate(prev_streak = lag(n = 1,streak)) 
            
            
            for (i in 1:120){
              DF_steps <- 
                DF_steps %>% 
                # mutate(streak = 0) %>% 
                mutate(prev_streak = lag(n = 1,streak))  %>% 
                mutate(streak = if_else(steps >= target, prev_streak +1, 0))
              
              
            }
            
            DF_steps %>% filter(streak == max(streak)) %>% mutate(target = target) %>% 
              rename(num_days = streak) %>% 
              select(date, num_days, target)
          }
          )
  )

DF <- DF %>% group_by(num_days, target) %>% summarise(date = max(date), .groups = "drop")



DF %>% 
  ggplot(aes(x = target, y = num_days)) + 
  geom_point(colour = "blue") + 
  ggtitle("longest streak of days with a minimum of X steps")


DF %>% 
  ggplot(aes(x = target, y = num_days, colour = substr(date,0,4))) + 
  geom_point() + 
  ggtitle("longest streak of days with a minimum of X steps")


DF %>% 
  ggplot(aes(x = target, y = num_days, colour = substr(date,0,7))) + 
  geom_point() + 
  ggtitle("longest streak of days with a minimum of X steps")



DF %>% ggplot(aes(x = target, y = date, colour = num_days)) + 
  # geom_point(colour = "blue") + 
  geom_point() + 
  ggtitle("longest streak of days with a minimum of X steps") 


DF %>% ggplot(aes(x = num_days, y = date, colour = target)) + 
  # geom_point(colour = "blue") + 
  geom_point() + 
  ggtitle("longest streak of days with a minimum of X steps") 

rbind(
  inner_join(DF, DF2, by = c("target")) %>% filter(num_days.x != num_days.y) %>% 
    select(target, num_days = num_days.x, date = date.x) %>% mutate(type = "new"),
  inner_join(DF, DF2, by = c("target")) %>% filter(num_days.x != num_days.y) %>% 
    select(target, num_days = num_days.y, date = date.y) %>% mutate(type = "old")) %>% View()
  ggplot(aes(x = target, y = num_days, colour = substr(date,0,7))) + 
  geom_point()


#########################################

DF_steps %>% mutate(gg = -rollmax(-steps, 28, align = "right", fill = "NA")) %>%
  ggplot(aes(x = date, y = gg)) + geom_point()



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


best_ndays_minsteps %>% 
  filter(min_steps < 15000) %>%
  filter(min_steps > 3000) %>%
  ggplot(aes(x = num_days, y = min_steps, colour = substr(date,0,4))) + 
  geom_point()


best_ndays_minsteps %>% 
  # filter(min_steps < 15000) %>% 
  # filter(min_steps > 5000) %>% 
  group_by(date,min_steps) %>% 
  summarise(n(), min(num_days), max(num_days)) %>% 
  View()



asfsf <- 
  do.call("rbind",
          lapply(seq(from = 1, to = 120, by = 1), function(num_days){
            
            DF_rolling <- 
              DF_steps %>% 
              mutate(rolling_min = -rollmax(-steps, num_days, align = "right", fill = "NA")) %>% 
              mutate(num_days = num_days)
            
            
          }))


asfsf %>% 
  # filter(date < '2025-06-01') %>%
  filter(!is.na(rolling_min)) %>%
  group_by(num_days) %>%  
  # summarise(asas = max(rolling_min)) %>% 
  filter(rolling_min == max(rolling_min)) %>%
  ungroup() %>%
  cross_join(data.frame(target = 1:25000)) %>%
  # cross_join(data.frame(target = 12000:13000)) %>%
  filter(target <= rolling_min) %>% 
  group_by(target) %>% 
  filter(num_days == max(num_days)) %>% 
  ungroup() %>% 
  group_by(target, num_days) %>% 
  filter(date == max(date)) %>% 
  group_by(date, rolling_min) %>%
  summarise(n(),min(num_days),max(num_days)) %>%
  View()
  # filter(rolling_min < 15000) %>% 
  ggplot(aes(x = target, y = num_days, colour = substr(date,0,4))) + 
  geom_point() + 
  ggtitle("longest streak of days with a minimum of X steps")


asfsf %>% 
  # filter(date < '2025-06-01') %>%
  filter(!is.na(rolling_min)) %>%
  group_by(num_days) %>%  
  # summarise(asas = max(rolling_min)) %>% 
  filter(rolling_min == max(rolling_min)) %>%
  ungroup() %>%
  # cross_join(data.frame(target = 10000:30000)) %>%
  cross_join(data.frame(target = 7500:15000)) %>%
  # cross_join(data.frame(target = 12000:13000)) %>%
  filter(target <= rolling_min) %>% 
  group_by(target) %>% 
  filter(num_days == max(num_days)) %>% 
  ungroup() %>% 
  group_by(target, num_days) %>% 
  filter(date == max(date)) %>% 
  # filter(rolling_min < 15000) %>% 
  ggplot(aes(x = target, y = num_days, colour = substr(date,0,7))) + 
  geom_point() + 
  ggtitle("longest streak of days with a minimum of X steps")




########### many times quicker and 10 times larger (don't need to go with steps of 10 anymore)
