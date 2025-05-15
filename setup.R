##### setup (using https://github.com/mrkaye97/fitbitr?tab=readme-ov-file)

# install.packages("fitbitr")
# install.packages("devtools")
# devtools::install_github("mrkaye97/fitbitr")

library(dplyr)
library(fitbitr)
library(httr)
library(jsonlite)


nima_login_data <- 
  read.csv("my_login_data.csv")

.fitbitr_token <- generate_fitbitr_token(
  app_name = nima_login_data$app_name,
  client_id = nima_login_data$client_id,
  client_secret = nima_login_data$client_secret,
  callback = nima_login_data$callback
)