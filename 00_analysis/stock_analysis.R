get_stock_list <-
function(stock_index){
  # APPLICATION: Pulls the names of all the stocks in the index
  tq_index(stock_index) %>% 
    select(symbol, company) %>% 
    arrange(symbol) %>% 
    mutate(label = str_c(symbol, company, sep = ", ")) %>% 
    select(label)
  
}
get_symbol_from_user_input <-
function(user_input, num = 1){
  # APPLICATION: Pulls the ticker from user's input 
  user_input %>% 
    str_split(pattern = ", ") %>% 
    pluck(1, num)
}

# Picking the first word of the company 
stock_list_tbl %>% 
  filter(str_detect(label, "Berkshire")) %>% 
  get_symbol_from_user_input(num = 2) %>% 
  str_split(pattern = " ") %>% 
  pluck(1, 1)


get_stock_prices <-
function(symbol, mavg_short = 5, mavg_long = 20, start = today() - years(5) + days(1), end = today()){
  # APPLICATION: Pulls the stock prices of each ticker
  symbol %>% 
    tq_get(get = "stock.prices",
           from = start,
           to = end) %>% 
    select(date, adjusted) %>% 
    mutate(mavg_short = rollmean(x = adjusted,
                                 k = mavg_short,
                                 fill = NA,
                                 align = "right")) %>% 
    mutate(mavg_long = rollmean(x = adjusted,
                                k = mavg_long,
                                fill = NA,
                                align = "right"))
  
  
}
get_stock_returns <-
function(data, mavg_short = 5, mavg_long = 20){
  # APPLICATION: Pulls the stock returns of each ticker from stock prices
  data %>% 
    tq_transmute(select = adjusted,
                 mutate_fun = periodReturn,
                 period = "daily",
                 col_rename = "returns") %>% 
    mutate(mavg_short = rollmean(x = returns,
                                 k = mavg_short,
                                 fill = NA,
                                 align = "right")) %>% 
    mutate(mavg_long = rollmean(x = returns,
                                k = mavg_long,
                                fill = NA,
                                align = "right"))
}
plot_data_prices <-
function(data, ggplot = TRUE){
  #APPLICATION: Plots prices with MA on ggplot and plotly
  p <- data %>% 
    pivot_longer(cols = adjusted:mavg_long,
                 names_to = "legend",
                 names_ptypes = list(legend = factor())) %>% 
    ggplot(aes(date,
               value,
               color = legend,
               group = legend)) + 
    geom_line(aes(linetype = legend)) +
    theme_tq() + 
    scale_color_tq() +
    scale_y_continuous(labels = scales::dollar_format(largest_with_cents = 10),
                       breaks = scales::pretty_breaks()) +
    labs(x = "",
         y = "Adjusted Share Price (incl dividends)")
    
  
  if(ggplot){
  ggplotly(p) %>% 
      layout(xaxis = list(
        rangeslider = list(type = "date")
      ))
  }

  
}
plot_data_returns <-
function(data, ggplot = TRUE){
  #APPLICATION: Plots returns with MA on ggplot and plotly
  p <- data %>% 
    # mutate(label_returns = scales::percent(returns,
    #                                  accuracy = 0.01),
    #        label_mavg_short = scales::percent(mavg_short,
    #                                     accuracy = 0.01),
    #        label_mavg_long = scales::percent(mavg_long,
    #                                    accuracy = 0.01)) %>%
    pivot_longer(cols = returns:mavg_long,
                 names_to = "legend",
                 names_ptypes = list(legend = factor())) %>% 
    ggplot(aes(date,
               value,
               color = legend,
               group = legend)) + 
    geom_line(aes(linetype = legend)) +
    theme_tq() + 
    scale_color_tq() +
    scale_y_continuous(labels = scales::percent_format()) +
    labs(x = "",
         y = "Adjusted Share Price (incl dividends)")
  
  if(ggplot){
    ggplotly(p) %>% 
      layout(xaxis = list(
        rangeslider = list(type = "date")
      ))
  }
  
  
}
generate_commentary <-
function(data, user_input){
  
  n_short <- data %>% 
    pull(mavg_short) %>% 
    is.na() %>% 
    sum() + 1
  
  n_long <- data %>% 
    pull(mavg_long) %>% 
    is.na() %>% 
    sum() + 1
  
  warning_signal <- data %>% 
    tail(1) %>% 
    mutate(mavg_warning_flag = mavg_short < mavg_long) %>% 
    pull(mavg_warning_flag)
  
  if(warning_signal){
    
    str_glue("In reviewing the stock prices of {user_input}, the {n_short}-day moving average is below the {n_long}-day moving average, indicating a negative trend")
  } else {
    
    str_glue("In reviewing the stock prices of {user_input}, the {n_short}-day moving average is above the {n_long}-day moving average, indicating a positive trend")
    
  }
  
  
  
  
}
