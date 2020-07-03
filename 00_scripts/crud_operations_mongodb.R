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
