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
    get_steps("2024-01-01", "2026-02-28"),
    get_steps("2022-01-01", "2023-12-31")#,
    # get_steps("2020-01-01", "2021-12-31"),
    # get_steps("2018-01-01", "2019-12-31"),
    # get_steps("2016-01-01", "2017-12-31")
  )



DF_calories <- 
  rbind(
    get_calories("2024-01-01", "2026-02-28"),
    get_calories("2022-01-01", "2023-12-31")#,
    # get_calories("2020-01-01", "2021-12-31"),
    # get_calories("2018-01-01", "2019-12-31"),
    # get_calories("2016-01-01", "2017-12-31")
  )

DF_both <- 
  inner_join(DF_steps, DF_calories, by = "date") %>% 
  filter(date > '2016-09-10') %>% 
  filter(date != '2024-12-28') %>% 
  filter(date != '2024-12-29') %>% 
  filter(date != '2024-12-30') %>% 
  filter(date != '2024-12-31')

DF_both %>% 
  ggplot(aes(x = steps, y = calories, colour = substr(date,0,4))) + 
  geom_point()



DF_both %>% 
  mutate(dd = substr(date,0,4)) %>% 
  ggplot(aes(x = steps, y = calories)) + 
  geom_point(colour = "blue") +
  facet_wrap(. ~ dd) +
  geom_smooth(method = "lm")




DF_both %>% 
  ggplot(aes(x = steps, y = calories)) + 
  geom_point(colour = "blue") +
  geom_smooth(method = "lm")


lm(calories ~ steps,
   DF_both %>% filter(date < '2025-04-01'))


DF_both %>% 
  mutate(res_sign = abs(calories - 2392 - steps * 0.08083)) %>% 
  mutate(res = calories - 2392 - steps * 0.08083) %>% 
  View()
