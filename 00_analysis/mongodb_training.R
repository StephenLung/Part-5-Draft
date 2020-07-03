# BUSINESS SCIENCE ----
# DS4B 202-R ----
# MONGO DB TRAINING -----
# Version 1

# LIBRARIES ----
library(mongolite) # Resource: https://jeroen.github.io/mongolite/
library(jsonlite)

library(config)

library(tidyverse)
library(lubridate)


# 1.0 CONNECTION TO REMOTE MONGODB ----

# Setup config Package & database YAML
Sys.setenv(R_CONFIG_ACTIVE = "default")
# Sys.getenv()

config <- config::get(file = "config.yml")

# Function to connect the config settings to mongo connection with:
# settings of username, password, host predefined in config yml 
mongo_connect <- function(collection, database, 
                          username = config$username, 
                          password = config$password, 
                          host = config$host){
    
    mongo(
        collection = collection,
        url = str_glue("mongodb+srv://{username}:{password}@{host}/{database}") # connection string
    )  
}


# Connect to MongoDB Atlas Cloud Database
mongo_connect(collection = "mtcars",
              database = "rstats")

# 2.0 ADD DATA ----

# Connect to collection
# setting up the connection
mongo_connection <- mongo_connect(collection = "mtcars",
                                  database = "rstats") 

# Adding data
# added 32 lines of data into the mongo db connection
# mtcars %>%
#     as_tibble(rownames = "model") %>%
#     mongo_connection$insert()

# 3.0 QUERYING DATA ----

# will return the db into a tibble
mongo_connection$find()

# limit search to the first 6 rows and converts into a prettified version of what a JSON file will look
mongo_connection$find(limit = 6) %>% 
    toJSON() %>% 
    prettify()

mongo_connection$find(query = '{"model": "Valiant"}') %>% #pulls as a dataframe
    as_tibble() #convert into a tibble

mongo_connection$count()

# 4.0 MODIFYING A COLLECTION ----

new_car_tbl <- tibble(
    model = "Ford F150",
    mpg   = 9.8,
    cyl   = 8,
    disp  = 275.8,
    hp    = 180,
    drat  = 3.07,
    wt    = 7.85,
    qsec  = 23.45,
    vs    = 0,
    am    = 1,
    gear  = 4,
    carb  = 3
)

new_car_tbl

# 4.1 Insert New Record
# insert a new record into the database
mongo_connection$insert(new_car_tbl) 

# counts number of records, should have increased by 1
mongo_connection$count()

# querying for the new record
mongo_connection$find(query = '{"model" : "Ford F150"}')

tibble(
    model = "Ford F150"
) %>% 
    toJSON() %>% 
    
    # ^ searches at start of string, $ searches at end of string, | stands for or \\ represents \
    str_remove_all(pattern = "^\\[|\\]$") %>% #regex to remove the '[]' brackets
    prettify() %>% # confirm it is removed
    mongo_connection$find(query = .) %>% 
    as_tibble()

# 4.2 Change a Record

# Update the record to reflect new value
mongo_connection$update(query = '{"model": "Ford F150"}', 
                        update = '{"$set" : {"mpg" : 10.8}}') 

# F250 does not exist in the library which will usually pop up an error
# upsert will allow you to insert and update into mongodb
mongo_connection$update(query = '{"model": "Ford F250"}', 
                        update = '{"$set" : {"mpg" : 10.8}}',
                        upsert = TRUE) 

mongo_connection$find()
mongo_connection$count()

mongo_connection$find() %>% as_tibble() %>% tail()

# 4.3 Remove a record


# 4.4 Remove entire table (be careful)




# 5.0 NESTED STRUCTURES ----

mongo_connection <- mongo_connect(
    database   = "stock_analyzer",
    collection = "user_base"
)

mongo_connection$drop()
mongo_connection$count()

user_base_tbl <- tibble(
    user           = c("user1", "user2"),
    password       = c("pass1", "pass2"), 
    permissions    = c("admin", "standard"),
    name           = c("User One", "User Two"),
    favorites      = list(c("AAPL", "GOOG", "NFLX"), c("MA", "V", "FB")),
    last_symbol    = c("GOOG", "NFLX"),
    user_settings  = list(tibble(mavg_short = 20, mavg_long = 50, time_window = 180), 
                          tibble(mavg_short = 30, mavg_long = 90, time_window = 365)),
    account_created = c(ymd_hms("2019-05-12 12:31:09"), ymd_hms("2019-06-04 06:18:02"))
) 

# Converting to JSON


# Adding nested structure to mongodb


# Retrieve - Preserves nested structure and format



# 6.0 STOCK ANALYZER APP - CRUD WORKFLOW -----

# Create new collection
mongo_connection <- mongo_connect(
    database   = "stock_analyzer",
    collection = "user_base"
)

# 6.1 Add User Data ----



# 6.2 Get User Data ----
# read_user_base <- function() {
#     user_base_tbl <<- read_rds(path = "00_data_local/user_base_tbl.rds")
# }



# 6.3 What shinyauthr does... ----



# 6.5 Update Mongo ----

# update_and_write_user_base <- function(user_name, column_name, assign_input) {
#     user_base_tbl[user_base_tbl$user == user_name, ][[column_name]] <<- assign_input
#     write_rds(user_base_tbl, path = "00_data_local/user_base_tbl.rds")
# }



# Before update


# After update


# 7.0 Save Functions ----

dump(c("mongo_connect", "mongo_get_user_base", "mongo_update_user_record"), 
     file = "00_scripts/crud_operations_mongodb.R", append = FALSE)


