# CRUD WORKFLOW ----

library(tidyverse) 
library(readr)

user_base_tbl <<- read_rds("00_data_local/user_base_tbl.rds")

# 1.0 WORKFLOW FOR CRUD OPERATIONS USING BASE R ----

# Replacing last symbol with MA for user1 
# First method is using base R, second method is using tidyverse 

## First Method - base R makes assignment easier 
# user_base_tbl[user_base_tbl$user == "user1", ][["last_symbol"]] <- "MA"

## Second Method
# user_base_tbl %>% 
#   mutate(last_symbol = case_when(
#     user == "user1" ~ "MA",
#     TRUE ~ last_symbol
#   ))

# Addition of new favourites of MA for user1 using base R
user_base_tbl[user_base_tbl$user == "user1",][["favourites"]] <- list(c("AAL", "DAL", "UA"))

user_base_tbl %>% filter(user == "user1") %>% pull(user_settings)

user_settings <- tibble(
  mavg_short = 15,
  mavg_long = 50,
  start_date = "2017-07-01",
  end_date = today()
)

# Replacing the user settings with tibble specified above 
user_settings
user_base_tbl[user_base_tbl$user == "user1",][["user_settings"]] <- list(user_settings)
user_base_tbl %>% filter(user == "user1") %>% pull(user_settings)

# Save as an RDS file 
write_rds(user_base_tbl, "00_data_local/user_base_tbl.rds")

read_rds(path = "00_data_local/user_base_tbl.rds") %>% 
  filter(user == "user1") %>% 
  pull(user_settings)

# 2.0 MODULARIZE FOR LOCAL STORAGE ----


read_user_base <- function(){
  # READ - Function saves the tibble into the global environment as opposed to within the function
  user_base_tbl <<- read_rds(path = "00_data_local/user_base_tbl.rds")
}

read_user_base()

# UPDATE - Assigning new entries into the global environment based on user, col_name and input
update_user_base <- function(user_name, column_name, assign_input){
  user_base_tbl[user_base_tbl$user == user_name,][[column_name]] <<-assign_input
}

update_user_base("user1", "name", "yes")
user_base_tbl

# WRITE - Save the updated tibble into an RDS file
write_user_base <- function(){
  write_rds(user_base_tbl, path = "00_data_local/user_base_tbl.rds")
}

write_user_base()
read_user_base()
user_base_tbl


update_and_write_user_base <- function(user_name, column_name, assign_input){
  # Combining the UPDATE and WRITE together in a function
  # UPDATE - Assigning new entries into the global environment based on user, col_name and input
  # WRITE - Save the updated tibble into an RDS file
  user_base_tbl[user_base_tbl$user == user_name,][[column_name]] <<-assign_input
  write_rds(user_base_tbl, path = "00_data_local/user_base_tbl.rds")
}

# 3.0 CHECK WORKFLOW ----

read_user_base()

user_base_tbl

update_and_write_user_base("user2", "name", "no")

rm(user_base_tbl)

read_user_base()
user_base_tbl %>% pull(favourites)

# 4.0 SAVE FUNCTIONS ----
dump(c("read_user_base", "update_and_write_user_base"), file = "00_scripts/crud_operations_local.R")

# 5.0 REVERT user_base_tbl ----
user_base_tbl <- tibble(
  user = c("user1", "user2"),
  password = c("pass1", "pass2"),
  permissions = c("admin", "standard"),
  name = c("User One", "User Two"),
  favourites = list(c("AAL", "DAL", "UAL"), c("MA", "V", "FB")),
  # last_symbol = c("GOOG", "NFLX"),
  user_settings = list(tibble(mavg_short = 20, mavg_long = 50, start_date = "2018-01-01", end_date = "2020-01-01"),
                       tibble(mavg_short = 30, mavg_long = 90, start_date = "2015-01-01", end_date = today()))
)
write_rds(user_base_tbl, path = "00_data_local/user_base_tbl.rds")

read_user_base()
