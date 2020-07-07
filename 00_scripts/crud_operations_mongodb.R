mongo_connect <-
function(collection, database, 
                          username = config$username, 
                          password = config$password, 
                          host = config$host){
    
    mongo(
        collection = collection,
        url = str_glue("mongodb+srv://{username}:{password}@{host}/{database}") # connection string
    )  
}
mongo_read_user_base <-
function(database = "portfolio_analyzer", collection = "user_base_test",
         username = config$username, 
         password = config$password, 
         host = config$host){
    
    mongo_connection <- mongo_connect(database = database,
                                      collection = collection,
                                      host = host,
                                      username = username,
                                      password = password)
    
    user_base_tbl <<- mongo_connection$find() %>% as_tibble()
    
    mongo_connection$disconnect()
    
}
mongo_update_and_write_user_base <-
function(user_name, column_name, assign_input,
         database = "portfolio_analyzer", 
         collection = "user_base_test",
         username = config$username, 
         password = config$password, 
         host = config$host) {
    
    user_base_tbl[user_base_tbl$user == user_name, ][[column_name]] <<- assign_input
    
    # Setting up the connection by stating the database, collection to update and
    # host, user and pass from the yaml config file
    mongo_connection <- mongo_connect(database = database,
                                      collection = collection,
                                      host = host,
                                      username = username,
                                      password = password)
    
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
