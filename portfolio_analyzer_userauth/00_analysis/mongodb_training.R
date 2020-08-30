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

# Removing a record and then view the updated tibble
mongo_connection$remove(query = '{"model": "Ford F250"}')
mongo_connection$find() %>% as_tibble() %>% tail()

# 4.4 Remove entire table (be careful)

mongo_connection$drop()
mongo_connection$find() %>% as_tibble() %>% tail()


# 4.5 Disconnecting from Database

mongo_connection$disconnect()

# 5.0 NESTED STRUCTURES ----

mongo_connection <- mongo_connect(
    database   = "portfolio_analyzer",
    collection = "user_base"
)

mongo_connection$drop()
mongo_connection$count()

user_base_tbl <- tibble(
    user            = c("user1", "user2"),
    password        = c("pass1", "pass2"), 
    permissions     = c("admin", "standard"),
    name            = c("User One", "User Two"),
    favourites      = list(c("AAPL", "GOOG", "NFLX"), c("MA", "V", "FB")),
    # last_symbol    = c("GOOG", "NFLX"),
    # convert to datetime format to fit POSIXt with either as_datetime or as_POSIXtc 
    user_settings   = list(tibble(mavg_short = 20, mavg_long = 50, start_date = as_datetime("2018-01-01"), end_date = as_datetime("2020-01-01")),
                         tibble(mavg_short = 30, mavg_long = 90, start_date = as_datetime("2015-01-01"), end_date = as_datetime(today()))),
    account_created = c(ymd_hms("2019-05-12 12:31:09"), ymd_hms("2019-06-04 06:18:02"))
) 

# Converting to JSON
user_base_tbl %>% 
    toJSON() %>% 
    prettify() 

user_base_tbl %>% 
    toJSON(POSIXt = "mongo") %>% # converting numeric value to a date in mongodb
    prettify() 


# Adding nested structure to mongodb
mongo_connection$insert(user_base_tbl)

# Retrieve - Preserves nested structure and format
mongo_connection$find() %>% as_tibble()


# 6.0 STOCK ANALYZER APP - CRUD WORKFLOW -----

# Create new collection
mongo_connection <- mongo_connect(
    database   = "portfolio_analyzer",
    collection = "user_base_test"
)

mongo_connection$drop()
mongo_connection$count()
mongo_connection$find()

# 6.1 Add User Data ----
mongo_connection$insert(user_base_tbl)


# 6.2 Get User Data ----
# read_user_base <- function() {
#     user_base_tbl <<- read_rds(path = "00_data_local/user_base_tbl.rds")
# }

mongo_read_user_base <- function(database = "portfolio_analyzer", collection = "user_base_test"){
    
    # establishes the connection to the database 
    mongo_connection <- mongo_connect(database = database,
                                      collection = collection,
                                      host = config$host,
                                      username = config$username,
                                      password = config$password)
    
    user_base_tbl <<- mongo_connection$find() %>% as_tibble()
    
    mongo_connection$disconnect()
    
}

rm(user_base_tbl)

mongo_read_user_base(database = "portfolio_analyzer",
                     collection = "user_base")


# 6.3 What shinyauthr does... ----

user_1_tbl <- user_base_tbl %>% 
    filter(
        user == "user1",
        password == "pass1"
    )


user_1_tbl %>% 
    pull(favourites)

pluck(user_1_tbl, "favourites", 1) <- c("AAPL", "GOOG", "NFLX", "ADBE")

# 6.5 Update Mongo ----

user_name <- "user2"
# mongo_connection$find(query = query_string)

mongo_update_and_write_user_base <- function(user_name, column_name, assign_input,
                                           database = "portfolio_analyzer", 
                                           collection = "user_base_test") {
    user_base_tbl[user_base_tbl$user == user_name, ][[column_name]] <<- assign_input
    
    # Setting up the connection by stating the database, collection to update and
    # host, user and pass from the yaml config file
    mongo_connection <- mongo_connect(database = database,
                                      collection = collection,
                                      host = config$host,
                                      username = config$username,
                                      password = config$password)
    
    # Query String
    # sets up the string to search in the mongo query
    query_string <- str_c('{"user": "', user_name, '"}')
    
    # Update String
    # Sets up the string in JSON to update with in mongo update
    update_string <- user_base_tbl %>% 
        filter(user == user_name) %>% 
        select(-user,-password, -permissions) %>%
        toJSON(POSIXt = "mongo") %>% 
        str_remove_all(pattern = "^\\[|\\]$")  #regex to remove the '[]' brackets
    # ^ matches start of the string, $ matches end of the string, \\ indicates special character 
        
    # Updates the table with the new entry 
    mongo_connection$update(query = query_string,
                            update = str_c('{"$set": ', update_string,' }') 
                            )
    
    # Disconnect
    mongo_connection$disconnect()
}



# Before update
mongo_connection$find()

user_base_tbl %>% 
    filter(user == "user1") %>% 
    pull(user_settings)

# After update
mongo_update_and_write_user_base(
    user_name = "user1",
    column_name = "user_settings",
    assign_input = list(tibble(
        mavg_short = 15,
        mavg_long = 75,
        start_date = as_datetime("2018-01-01"),
        end_date = as_datetime("2020-01-01")
    ))
)

mongo_connection$find()

# 7.0 Save Functions ----

dump(c("mongo_connect", "mongo_read_user_base", "mongo_update_and_write_user_base"), 
     file = "00_scripts/crud_operations_mongodb.R", append = FALSE)


# 8.0 Addition of stocks in user_base_tbl ----

# Created new user_base_tbl with the portfolio tickers
user_base_tbl <- tibble(
    user            = c("user1", "user2"),
    password        = c("pass1", "pass2"), 
    permissions     = c("admin", "standard"),
    name            = c("User One", "User Two"),
    favourites      = list(c("FB", "MSFT", "NFLX", "AMZN"), c("MA", "V", "AXP", "COF")),
    portfolio       = list(c("FB", "MSFT", "NFLX", "AMZN"), c("MA", "V", "AXP", "COF")),
    # last_symbol    = c("GOOG", "NFLX"),
    # convert to datetime format to fit POSIXt with either as_datetime or as_POSIXtc 
    user_settings   = list(tibble(mavg_short = 20, mavg_long = 50, start_date = as_datetime("2018-01-01"), end_date = as_datetime("2020-01-01")),
                           tibble(mavg_short = 30, mavg_long = 90, start_date = as_datetime("2015-01-01"), end_date = as_datetime(today()))),
    account_created = c(ymd_hms("2019-05-12 12:31:09"), ymd_hms("2019-06-04 06:18:02"))
) 

# 8.1 STOCK ANALYZER APP - CRUD WORKFLOW -----
# Create new connection with a new database
mongo_connection <- mongo_connect(
    database   = "portfolio_analyzer_2",
    collection = "user_base_test"
)

# 8.2 Add User Data ----
# Adding tibble into the database
# mongo_connection$drop()
mongo_connection$insert(user_base_tbl)

mongo_connection$disconnect()

# Replacing existing database with original user_base_tbl


# Testing of user_base_tbl and pulling the portfolio list
user_base_tbl %>% 
    filter(user == "user1") %>% 
    select(portfolio) %>% unlist() %>% .[4]
