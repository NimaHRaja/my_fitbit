library(dplyr)
library(fitbitr)
library(ggplot2)

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
    get_steps("2024-01-01", "2025-05-30"),
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
  filter(mean_steps < 15000) %>% 
  ggplot(aes(x = num_days, y = mean_steps)) + 
  geom_point(colour = "blue")


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
  geom_smooth()



DF_steps %>% 
  mutate(b = lag(n = 28,a)) %>%
  mutate(mean_steps = (a - b)/28) %>% 
  mutate(mean_steps = if_else(is.na(mean_steps),a/28,mean_steps)) %>%
  filter(date > '2016-10-08') %>% 
  ggplot(aes(x = date, y = mean_steps)) + 
  geom_point(colour = "blue") + 
  geom_smooth()


DF_steps %>% 
  ggplot(aes(x = date, y = steps)) + 
  geom_smooth()


DF_steps %>% 
  ggplot(aes(x = date, y = steps)) + 
  geom_point(colour = "blue") +
  geom_hline(yintercept = 10000, colour = "red") +
  geom_hline(yintercept = 11111, colour = "red") +
  geom_hline(yintercept = 12000, colour = "red") +
  geom_hline(yintercept = 12500, colour = "red")
