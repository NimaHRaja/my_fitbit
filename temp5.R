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
    get_steps("2026-01-02", "2026-10-25"),
    get_steps("2024-01-01", "2026-01-01"),
    get_steps("2022-01-01", "2023-12-31"),
    get_steps("2020-01-01", "2021-12-31"),
    get_steps("2018-01-01", "2019-12-31"),
    get_steps("2016-01-01", "2017-12-31")
  )


DF_steps %>% 
  arrange(date) %>% mutate(ssss = rollsum(if_else(steps>=13000,1,0), k = 28, fill = NA, align = "right")) %>%
  ggplot(aes(x = date, y = ssss)) + geom_point(colour = "blue")


DF_steps %>% 
  arrange(date) %>% mutate(ssss = rollsum(if_else(steps>=13000,1,0), k = 7, fill = NA, align = "right")) %>%
  ggplot(aes(x = date, y = ssss)) + geom_point(colour = "blue")


DF_steps %>% 
  arrange(date) %>% mutate(ssss = rollsum(if_else(steps>=10000,1,0), k = 28, fill = NA, align = "right")) %>%
  ggplot(aes(x = date, y = ssss)) + geom_point(colour = "blue")

DF_steps %>% 
  arrange(date) %>% mutate(ssss = rollsum(if_else(steps>=20000,1,0), k = 28, fill = NA, align = "right")) %>%
  ggplot(aes(x = date, y = ssss)) + geom_point(colour = "blue")


DF_steps %>% 
  arrange(date) %>% mutate(ssss = rollsum(if_else(steps>=13000,1,0), k = 365, fill = NA, align = "right")) %>%
  ggplot(aes(x = date, y = ssss)) + geom_point(colour = "blue")


DF_steps %>% 
  arrange(date) %>% mutate(ssss = rollsum(if_else(steps>=10000,1,0)/365, k = 365, fill = NA, align = "right")) %>%
  ggplot(aes(x = date, y = ssss)) + geom_point(colour = "blue")

DF_steps %>% 
  arrange(date) %>% mutate(ssss = rollsum(if_else(steps>=10000,1,0)/365, k = 365, fill = NA, align = "right")) %>%
  View()

###################

DF_steps %>% 
  arrange(date) %>% mutate(ssss = rollmax(-steps, k = 365, fill = NA, align = "right")) %>% 
  View()



