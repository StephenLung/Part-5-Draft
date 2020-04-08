get_stock_list <-
function(stock_index){
  
  tq_index(stock_index) %>% 
    select(symbol, company) %>% 
    arrange(symbol) %>% 
    mutate(label = str_c(symbol, company, sep = ", ")) %>% 
    select(label)
  
}
get_symbol_from_user_input <-
function(user_input){
  # APPLICATION: Pulls the ticker from user's input 
  user_input %>% 
    str_split(pattern = ", ") %>% 
    pluck(1,1)
  }
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
    scale_color_tq()
  
  if(ggplot){
  ggplotly(p)
  }

  
}
plot_data_returns <-
function(data, ggplot = TRUE){
  #APPLICATION: Plots returns with MA on ggplot and plotly
  p <- data %>% 
    # mutate(returns = scales::percent(returns,
    #                                  accuracy = 0.01),
    #        mavg_short = scales::percent(mavg_short,
    #                                     accuracy = 0.01),
    #        mavg_long = scales::percent(mavg_long,
    #                                    accuracy = 0.01)) %>% 
    pivot_longer(cols = returns:mavg_long,
                 names_to = "legend",
                 names_ptypes = list(legend = factor())) %>% 
    ggplot(aes(date,
               value,
               color = legend,
               group = legend)) + 
    geom_line(aes(linetype = legend)) +
    scale_y_continuous(labels = scales::percent_format()) +
    theme_tq() + 
    scale_color_tq()
  
  if(ggplot){
  ggplotly(p)
  }
  
  
}
