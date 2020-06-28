library(lubridate)
library(tidyverse)

# 1.0 WORKFLOW FOR CRUD OPERATIONS USING BASE R ----
user_base_tbl <- tibble(
    user = c("user1", "user2"),
    password = c("pass1", "pass2"),
    permissions = c("admin", "standard"),
    name = c("User One", "User Two"),
    favourites = list(c("AAL", "DAL", "UAL"), c("MA", "V", "FB")),
    # last_symbol = c("GOOG", "NFLX"),
    user_settings = list(tibble(mavg_short = 20, mavg_long = 50, start_date = "2018-01-01", end_date = "2020-01-01"),
                         tibble(mavg_short = 30, mavg_long = 90, start_date = "2015-01-01", end_date = today())),
    account_created = c("2019-05-12 12:31:09", "2019-06-04 06:18:02") %>% ymd_hms()
  )

# Assigning additional variables to the favourites list
user_base_tbl[user_base_tbl$user == "user1", ][["favourites"]] <- list(c("AAPL", "GOOG", "NFLX", "MA"))

write_rds(user_base_tbl, path = "00_data_local/user_base_tbl.rds")
read_rds(path = "00_data_local/user_base_tbl.rds")

# 2.0 MODULARIZE FOR LOCAL STORAGE

read_user_base <- function(){
  user_base_tbl <<- read_rds(path = "00_data_local/user_base_tbl.rds")
  
}

read_user_base()
