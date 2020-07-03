multi_asset_price_portfolio <-
  function(symbols, end, start, wts_tbl){
    
    if(length(symbols) == 1){
      # Download Data
      download_data <- symbols %>% 
        tq_get(get = "stock.prices",
               from = start,
               to = end) 
      
      stock_data <- download_data %>%
        select(date, adjusted) %>% 
        add_column(symbol = symbols,
                   .before = TRUE)
      
      return(stock_data)
    }
    
    else if(length(symbols) > 1){
      
      # Download Data
      download_data <- symbols %>% 
        tq_get(get = "stock.prices",
               from = start,
               to = end) 
      
      # Determine earliest date with full set of data
      min_date <- download_data %>%   
        group_by(symbol) %>% 
        summarise(min_date = min(date), 
                  max_date = max(date)) %>% 
        ungroup() %>% 
        pull(min_date) %>% 
        max()
      
      stock_data <- download_data %>%
        select(symbol, date, adjusted) %>% 
        filter(date >= min_date)
      
      return(stock_data)
      
    }
  }

user_base_tbl %>% filter(user == "user1") %>% pull(user_settings)
symbols <-  c("AAPL", "MSFT", "NFLX", "AMZN")
end <-  user_base_tbl %>% filter(user == "user1") %>% pull(user_settings) %>% pluck(1) %>% pull(end_date)
start <- user_base_tbl %>% filter(user == "user1") %>% pull(user_settings) %>% pluck(1) %>% pull(start_date)
w       <- c(0.25,
             0.25,
             0.25,
             0.25)
wts_tbl <- tibble(symbols, w)
mavg_short <- user_base_tbl %>% filter(user == "user1") %>% pull(user_settings) %>% pluck(1) %>% pull(mavg_short)
mavg_long <- user_base_tbl %>% filter(user == "user1") %>% pull(user_settings) %>% pluck(1) %>% pull(mavg_long)

data <- multi_asset_price_portfolio(symbols = symbols,
                            end = end,
                            start = start,
                            wts_tbl = wts_tbl) %>% # grabs list of stocks
  multi_asset_return_portfolio(period = "monthly") %>% # convert prices to returns
  wealth_index(wts_tbl = wts_tbl, name_portfolio = "test portfolio") %>% # build a wealth index
  portfolio_mavg_calculation(mavg_short = mavg_short, # calculate moving avg 
                             mavg_long = mavg_long)

short_window <- data %>% 
  pull(mavg_short) %>% 
  is.na() %>% 
  sum() + 1

long_window <- data %>% 
  pull(mavg_long) %>% 
  is.na() %>% 
  sum() + 1

warning_flag <- data %>%
  tail(1) %>%
  mutate(flag = mavg_short > mavg_long) %>%
  pull(flag)

is.na(warning_flag)
