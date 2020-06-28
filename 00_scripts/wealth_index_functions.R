single_asset_price <-
function(symbols, end, start){
  
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
multi_asset_return_portfolio <-
function(stock_price_tbl, period = "monthly"){
    
    # Transform to Returns
    period_return_tbl <- stock_price_tbl %>% 
      select(symbol, date, adjusted) %>% 
      group_by(symbol) %>% 
      tq_transmute(select     = adjusted,
                   mutate_fun = periodReturn,
                   period     = period,
                   col_rename = "returns") %>% 
      ungroup()
    
    
    # if statement, monthly or longer period will adjust rollback date
    period_return_tbl <- if(period == "daily"){
      
      period_return_tbl
      
    } else if(period == "weekly"){
      
      period_return_tbl      
      
    }
    
    else{
      
      period_return_tbl %>% 
        #rollback to first day of the month - ETF Issue ----
      mutate(date = lubridate::rollback(date, roll_to_first = TRUE))
      
    }

    
    return(period_return_tbl)
    
    
}
wealth_index <-
function(return_data, wts_tbl, name_portfolio){
  
  name_portfolio <- as_name(name_portfolio)
  
  # Determine # of tickers
  symbol_length <- return_data %>% 
    select(symbol) %>% 
    unique() %>% 
    pull(symbol)
  
  # One ticker will not require table weights for wealth index
  if(length(symbol_length) == 1){
    
    return_data %>% 
      tq_portfolio(assets_col = symbol,
                   returns_col = returns,
                   wealth.index = TRUE) %>% 
      mutate(investment.growth = portfolio.wealthindex * 10000) %>% 
      add_column(portfolio = name_portfolio, .before = 1) 
      #added mavg 04.14.2020 ----
      # mutate(mavg_short = rollmean(investment.growth, k = mavg_short, fill = NA, align = "right"),
             # mavg_long = rollmean(investment.growth, k = mavg_long, fill = NA, align = "right")) 
    
  }
  
  # 1+ ticker will require table weights for wealth index
  else if(length(symbol_length) > 1){
    
    return_data %>% 
      tq_portfolio(assets_col = symbol,
                   returns_col = returns,
                   weights = wts_tbl,
                   wealth.index = TRUE) %>% 
      mutate(investment.growth = portfolio.wealthindex * 10000) %>% 
      add_column(portfolio = name_portfolio, .before = 1)
      #added mavg 04.14.2020 ---- 
      # mutate(mavg_short = rollmean(investment.growth, k = mavg_short, fill = NA, align = "right"),
             # mavg_long = rollmean(investment.growth, k = mavg_long, fill = NA, align = "right")) 
  }

}
bind_portfolio <-
function(data, data_2){
  
  data %>% 
    bind_rows(data_2)
  
}
plot_portfolio <-
function(data, interactive = TRUE, theme = theme_tq()){
  
  theme_set(theme)
  
  
  g <- data %>% 
    mutate(portfolio = as.factor(portfolio) %>% fct_reorder(investment.growth),
           label_text = str_glue("Portfolio: {str_to_title(portfolio)}
                                 Investment: {scales::dollar(investment.growth, accuracy = 1)}
                                 Growth %: {scales::percent(portfolio.wealthindex - 1, accuracy = 0.01)}")) %>% 
    ggplot(aes(x = date, y = investment.growth, col = portfolio)) +
    geom_point(aes(text = label_text), size = 0.1) + #Must indicate label_text for tooltip to showup 
    geom_line() + 
    
    # Addition of Global Financial Crisis vertical line in Sept 2008
    geom_vline(xintercept = as.numeric(as.Date("2008-09-01")), linetype = "dotted", color = "red", size = 1.5) +
    annotate("text", x =  as.Date("2008-09-01") + 1200, y = 23000, label = "2008 Financial Crisis", color = "red") + 

    scale_color_tq() + 
    scale_y_continuous(labels = scales::dollar_format()) + 
    labs(title = "Portfolio vs Benchmark (SP500)",
         x = "",
         y = "")
  
  if(interactive){
    
    return(ggplotly(g, tooltip = "text"))
    
  } else{
    
    return(g)
  }
  
}
format_table <-
function(symbols, w){
  
  tibble(symbols, w) %>% 
    `colnames<-`(c("Ticker", "Weights")) %>% 
    mutate(Weights = scales::percent(Weights, 
                                    accuracy = 0.1))
}
portfolio_return <-
function(return_data, wts_tbl, name_portfolio, rebalance = "years"){
  
  # name_portfolio <- as_name(name_portfolio) #not necessary?
  
  # Determine # of tickers
  symbol_length <- return_data %>% 
    select(symbol) %>% 
    unique() %>% 
    pull(symbol)
  
  # One ticker will not require table weights for portfolio return
  if(length(symbol_length) == 1){
    
    return_data %>% 
      mutate(symbol = name_portfolio) 
  }
  
  # 1+ ticker will require table weights for portfolio return
  else {
    
    return_data %>% 
      tq_portfolio(assets_col = symbol,
                   returns_col = returns,
                   weights = wts_tbl,
                   rebalance_on = rebalance,
                   col_rename = "returns") %>% 
      add_column(symbol = name_portfolio, .before = 1) 
  }
  
}
rolling_skew_tq <-
function(symbols, end, start, wts_tbl, name_portfolio, window){
  
  multi_asset_price_portfolio(symbols, end, start, wts_tbl) %>%
    multi_asset_return_portfolio(period = "monthly") %>%
    portfolio_return(wts_tbl = wts_tbl,
                     name_portfolio = name_portfolio,
                     rebalance = "years") %>%
    tq_mutate(select = returns,
              mutate_fun = rollapply,
              width = window,
              FUN = skewness,
              col_rename = "skewness") %>% 
    na.omit() %>% 
    select(-returns) %>% 
    tk_xts(date_var = date)
}
rolling_kurt_tq <-
function(symbols, end, start, wts_tbl, name_portfolio, window){
  
  multi_asset_price_portfolio(symbols, end, start, wts_tbl) %>% 
  multi_asset_return_portfolio(period = "monthly") %>% 
  portfolio_return(wts_tbl = wts_tbl, 
                   name_portfolio = name_portfolio,
                   rebalance = "years") %>% 
  tq_mutate(select = returns,
            mutate_fun = rollapply,
            width = window,
            FUN = kurtosis,
            col_rename = "kurtosis") %>% 
  na.omit() %>% 
    select(-returns) %>% 
    tk_xts(date_var = date)
}
sharpe_ratio_tq <-
function(symbols, end, start, wts_tbl, rfr = rfr, name_portfolio, name_ratio = "sharpe_ratio"){

  multi_asset_price_portfolio(symbols, end, start, wts_tbl) %>% 
    multi_asset_return_portfolio(period = "monthly") %>% 
    portfolio_return(wts_tbl = wts_tbl,
                     name_portfolio = name_portfolio,
                     rebalance = "years") %>% 
    tq_performance(Ra = returns,
                   performance_fun = SharpeRatio,
                   Rf = rfr,
                   FUN = "StdDev") %>%
    `colnames<-`(name_ratio)
  
}
sharpe_tq_2 <-
function(df){
  SharpeRatio(df,
              Rf = rfr,
              FUN = "StdDev")
}
sharpe_tq_roll <-
function(symbols, end, start, wts_tbl, name_portfolio, name_ratio = "sharpe_ratio"){

  multi_asset_price_portfolio(symbols, end, start, wts_tbl) %>%
    multi_asset_return_portfolio(period = "monthly") %>%
    portfolio_return(wts_tbl = wts_tbl,
                     name_portfolio = name_portfolio,
                     rebalance = "years") %>%
    tq_mutate(
      select = returns,
      mutate_fun = rollapply,
      width = window,
      align = "right",
      FUN = sharpe_tq_2,
      col_rename = "sharpe_ratio") %>%
    na.omit()

}
