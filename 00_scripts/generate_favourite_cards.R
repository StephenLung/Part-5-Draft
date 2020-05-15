get_stock_mavg_info <-
function(data){
  
  n_short <- data %>% 
    pull(mavg_short) %>% # Pulls mavg_short column
    is.na() %>% # Counts number of NAs
    sum() + 1 # Adds 1 to get to the total
  n_long <- data %>% 
    pull(mavg_long) %>% 
    is.na() %>% 
    sum() + 1
  
  data %>% 
    tail(1) %>% 
    mutate(mavg_flag = mavg_short > mavg_long, # Creates a boolean flag
           n_short = n_short, # Adds the short window
           n_long = n_long, # Adds the long window 
           pct_change = (mavg_short - mavg_long)/mavg_long) # Adds the percentage change
}
