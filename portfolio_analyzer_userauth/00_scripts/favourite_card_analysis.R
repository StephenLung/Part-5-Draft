library(pacman)
p_load(
  tidyquant,
  tidyverse
)

source("00_scripts/stock_analysis_functions.R")
source("00_scripts/info_card.R")

favourite_list_on_start <- c("AAPL", "MSFT", "NFLX", "AMZN")
stock_list_tbl <- get_stock_list("SP500")

stock_data_favourites_tbl <- favourite_list_on_start %>% 
  map(.f = get_stock_prices) %>%             # Applies function to each ticker
  set_names(favourite_list_on_start)         # Renames the list with stock

data <- stock_data_favourites_tbl$AAPL
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




# Get Moving Average per stock history ----
get_stock_mavg_info <- function(data){
  
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


# Generate Favourite Card ----
generate_favourite_card <- function(data){
  column(width = 3, 
         info_card(
           title = as.character(data$stock),
           value = str_glue("{data$n_short} <small>vs {data$n_long}</small>") %>% HTML(),
           sub_value = data$pct_change %>% scales::percent(accuracy = 0.01),
           sub_text_color = ifelse(data$mavg_flag, "success", "danger"),
           sub_icon = ifelse(data$mavg_flag, "arrow-up", "arrow-down")
         ))
}

# # purrr:map with anonymous function form
# favourite_list_on_start %>% 
#   map(~get_stock_prices(.x, mavg_short = 3, mavg_long = 5))

# FULL codeflow to get from ticker symbol to favourite card ----
favourite_list_on_start %>%
  
  #Pull the list of stock data with mavg
  map(.f = function(x){
    x %>% 
      get_stock_prices(
        mavg_short = 10,
        mavg_long = 20,
        start = today() - years(5),
        end = today()
      )
  }) %>% 
  
  #Get last row of the data 
  map(.f = function(x){
    x %>% 
      get_stock_mavg_info()
  }) %>% 
  set_names(favourite_list_on_start) %>% 
  
  #Add in stock as a column, retain position and keep as list for subsequent map
  bind_rows(.id = "stock") %>% 
  mutate(stock = as_factor(stock)) %>% 
  split(.$stock) %>% 
  
  
  #Add favourites cards
  map(.f = function(data){
    data %>% 
      generate_favourite_card()
    }) %>% 
  tagList()
  
  
# Generate Favourite Card FINAL ----
generate_favourite_cards <- function(favourites_ticker,
                                     mavg_short = 30,
                                     mavg_long = 90,
                                     start = today() - years(5),
                                     end = today()){
  
  favourites_ticker %>%
    
    #Pull the list of stock data with mavg
    map(.f = function(x){
      x %>% 
        get_stock_prices(
          mavg_short = mavg_short,
          mavg_long = mavg_long,
          start = start,
          end = end
        )
    }) %>% 
    
    #Get last row of the data 
    map(.f = function(x){
      x %>% 
        get_stock_mavg_info()
    }) %>% 
    set_names(favourites_ticker) %>% 
    
    #Add in stock as a column, retain position and keep as list for subsequent map
    bind_rows(.id = "stock") %>% 
    mutate(stock = as_factor(stock)) %>% 
    split(.$stock) %>% 
    
    
    #Add favourites cards
    map(.f = function(data){
      data %>% 
        generate_favourite_card()
    }) %>% 
    tagList()
  
  
}









dump(c("get_stock_mavg_info", "generate_favourite_card", "generate_favourite_cards"),
     file = "00_scripts/generate_favourite_cards.R", append = FALSE)
