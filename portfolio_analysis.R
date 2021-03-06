# setwd("C:/Users/steph/Dropbox/Business University Science/Personal Portfolio/Part-5-Draft")
library(pacman)

# Shiny
library(flexdashboard)
library(shiny)
library(shinyjs)
library(shinyWidgets)
library(shinythemes)
library(shinyauthr)

# Core
library(tidyquant)
library(tidyverse)
library(timetk)
library(tidyr)
library(tibble)
library(dplyr)
library(furrr)
library(purrr)
library(glue)
library(forcats)
library(stringr)
library(rlang)

# Text
library(tidytext)    # Tidy text mining
library(textdata)    # Needed for AFINN

# Visualizations
library(plotly)
library(highcharter)
library(correlationfunnel)
library(ggwordcloud) # Extension for wordclouds

# Interactive Maps
library(tmaptools)
library(leaflet) 

# Twitter API
library(rtweet) 

# Modeling
library(parsnip)
library(xgboost)
library(glmnet)

source("00_scripts/stock_analysis_functions.R")
source("00_scripts/portfolio_analysis_functions.R")
source("00_prior_script/wealth_index.R")
source("00_scripts/info_card.R")
source("00_scripts/generate_favourite_cards.R")
source("00_scripts/geocode_for_free.R") #geocoding for locations

# INPUT DATA ----
symbols <- c("AAPL",
             "MSFT",
             "NFLX",
             "AMZN")

end     <- today()
start   <- end - years(5) + days(1)

w <- c(0.25, 0.25, 0.25, 0.25)

wts_tbl <- tibble(symbols, w)

source("00_prior_script/wealth_index.R")

# 1.0 GET STOCK DATA ----
portfolio_price_data <- multi_asset_price_portfolio(symbols, end, start, wts_tbl) 

# 1.1 PLOT DATA ----

q <- portfolio_price_data %>% 
  mutate(label_text = str_glue("Date: {date}
                               Price: {scales::dollar(adjusted)}")) %>% 
  mutate(symbol = fct_reorder(symbol, desc(adjusted))) %>% 
  ggplot(aes(date, adjusted, colour = symbol, group = symbol)) + 
  geom_point(aes(text = label_text), size = 0.1)+
  geom_line(lwd = 0.5) +
  theme_tq() + 
  theme(legend.position = "right") +
  scale_color_tq() + 
  scale_y_continuous(labels = scales::dollar_format(largest_with_cents = 10),
                     breaks = scales::pretty_breaks()) + 
  labs(title = str_glue("Stock prices of the following: PLACEHOLDER"),
       x = "",
       y = "Adjusted Stock Price (incl dividends)")

ggplotly(q, tooltip = "text")

plot_portfolio_price <- function(data, ggplot = TRUE){
  
  p <- data %>% 
    mutate(label_text = str_glue("Symbol: {symbol}
                               Date: {date}
                               Price: {scales::dollar(adjusted)}")) %>% 
    mutate(symbol = fct_reorder(symbol, desc(adjusted))) %>% 
    ggplot(aes(date, adjusted, colour = symbol, group = symbol)) + 
    geom_point(aes(text = label_text), size = 0.1)+
    geom_line(lwd = 0.5) +
    theme_tq() + 
    theme(legend.position = "right") +
    scale_color_tq() + 
    scale_y_continuous(labels = scales::dollar_format(largest_with_cents = 10),
                       breaks = scales::pretty_breaks()) + 
    labs(title = str_glue("Stock prices of the following: PLACEHOLDER"),
         x = "",
         y = "Adjusted Stock Price (incl dividends)")
  
  if(ggplot){
    ggplotly(p, tooltip = "text") %>% 
      layout(xaxis = list(
        rangeslider = list(type = "date")
      ))
  }
  
}

portfolio_price_data %>% 
  plot_portfolio_price()

# 2.0 PORTFOLIO DATA ----

portfolio_data_tbl <- multi_asset_price_portfolio(symbols, end, start, wts_tbl) %>% 
  multi_asset_return_portfolio(period = "monthly") %>% 
  wealth_index(wts_tbl = wts_tbl, name_portfolio = "test portfolio") %>% 
  mutate(mavg_short = rollmean(investment.growth, k = 5, fill = NA, align = "right"),
         mavg_long = rollmean(investment.growth, k = 20, fill = NA, align = "right")) 

portfolio_data_tbl <- multi_asset_price_portfolio(symbols, end, start, wts_tbl) %>% 
  multi_asset_return_portfolio(period = "monthly") %>% 
  wealth_index(wts_tbl = wts_tbl, name_portfolio = "test portfolio", mavg_short = 20, mavg_long = 50)

p <- portfolio_data_tbl %>% 
  
  mutate(label_text = str_glue("Date: {date}
                               Investment: {scales::dollar(investment.growth)}
                               Growth %: {scales::percent(portfolio.wealthindex-1, accuracy = 0.01)}
                               SMA: {scales::dollar(mavg_short)}
                               LMA: {scales::dollar(mavg_long)}")) %>%

  pivot_longer(cols = investment.growth:mavg_long, names_to = "legend", values_to = "value",
              names_ptypes = list(legend = factor(levels = c("investment.growth", 
                                                             "mavg_short",
                                                             "mavg_long")))) %>% 

  ggplot(aes(date, value, color = legend, group = legend)) + 
  geom_point(aes(text = label_text), size = 0.1)+
  geom_line(lwd = 1.0) +
  theme_tq() + 
  theme(legend.position = "right") +
  scale_color_tq() + 
  scale_y_continuous(labels = scales::dollar_format(largest_with_cents = 10),
                     breaks = scales::pretty_breaks()) + 
  labs(title = str_glue("Portfolio Growth"),
       x = "",
       y = "Adjusted Portfolio Value (incl dividends)")

ggplotly(p, tooltip = "text")


plot_portfolio_index <- function(data, ggplot = TRUE){
  
  p <- data %>% 
    mutate(label_text = str_glue("Date: {date}
                               Investment: {scales::dollar(investment.growth)}
                               Growth %: {scales::percent(portfolio.wealthindex-1, accuracy = 0.01)}")) %>% 
    
    # pivot_longer(cols = investment.growth:mavg_long, names_to = "legend", values_to = "value",
    #              names_ptypes = list(legend = factor(levels = c("investment.growth", 
    #                                                             "mavg_short",
    #                                                             "mavg_long")))) %>% 
    
    ggplot(aes(date, investment.growth)) + 
    geom_point(aes(text = label_text), size = 0.1)+
    geom_line(lwd = 1.0) +
    theme_tq() + 
    theme(legend.position = "right") +
    scale_color_tq() + 
    scale_y_continuous(labels = scales::dollar_format(largest_with_cents = 10),
                       breaks = scales::pretty_breaks()) + 
    labs(title = str_glue("Portfolio Growth"),
         x = "",
         y = "Adjusted Portfolio Value (incl dividends)")
  
  if(ggplot){
    ggplotly(p, tooltip = "text") %>% 
      layout(xaxis = list(
        rangeslider = list(type = "date")
      ))
  }
  
}

portfolio_data_tbl %>% 
  plot_portfolio_index()

# 2.1 PORTFOLIO DATA WITH MAVG ----

mavg_calculation <- function(data, mavg_short = 20,
                             mavg_long = 50){
  
  data %>% 
    mutate(mavg_short = rollmean(investment.growth, k = mavg_short, fill = NA, align = "right"),
           mavg_long = rollmean(investment.growth, k = mavg_long, fill = NA, align = "right"))
  
}

plot_portfolio_index_mavg <- function(data, ggplot = TRUE){
  
  p <- data %>%

    # mutate(mavg_short = rollmean(investment.growth, k = mavg_short, fill = NA, align = "right"),
    # mavg_long = rollmean(investment.growth, k = mavg_long, fill = NA, align = "right")) %>% 
    mutate(label_text = str_glue("Date: {date}
                               Investment: {scales::dollar(investment.growth)}
                               Growth %: {scales::percent(portfolio.wealthindex-1, accuracy = 0.01)}
                               SMA: {scales::dollar(mavg_short)}
                               LMA: {scales::dollar(mavg_long)}")) %>% 
    pivot_longer(cols = investment.growth:mavg_long, names_to = "legend", values_to = "value",
                 names_ptypes = list(legend = factor(levels = c("investment.growth",
                                                                "mavg_short",
                                                                "mavg_long")))) %>% 
    ggplot(aes(date, value, color = legend, group = legend)) +
    geom_point(aes(text = label_text), size = 0.1) + 
    geom_line(lwd = 1) + 
    theme_tq() + 
    theme(legend.position = "right") +
    scale_color_tq() +
    scale_y_continuous(labels = scales::dollar_format(largest_with_cents = 10),
                       breaks = scales::pretty_breaks()) + 
    labs(title = str_glue("Portfolio Growth"),
         x = "",
         y = "Adjusted Portfolio Value (incl dividends)")
  
  if(ggplot){
    ggplotly(p, tooltip = "text") %>% 
      layout(xaxis = list(
        rangeslider = list(type = "date")
      ))
  }
    
  
}

multi_asset_price_portfolio(symbols, end, start, wts_tbl) %>% 
  multi_asset_return_portfolio(period = "monthly") %>% 
  wealth_index(wts_tbl = wts_tbl, name_portfolio = "test portfolio") %>% 
  mavg_calculation() %>% 
  plot_portfolio_index_mavg()

# 3.0 PORTFOLIO COMMENTARY ----

portfolio_data_mavg_tbl <- multi_asset_price_portfolio(symbols, end, start, wts_tbl) %>% 
  multi_asset_return_portfolio(period = "monthly") %>% 
  wealth_index(wts_tbl = wts_tbl, name_portfolio = "test portfolio") %>% 
  mavg_calculation()

short_window <- portfolio_data_mavg_tbl %>% 
  pull(mavg_short) %>% 
  is.na() %>% 
  sum() + 1

long_window <- portfolio_data_mavg_tbl %>% 
  pull(mavg_long) %>% 
  is.na() %>% 
  sum() + 1

warning_flag <- portfolio_data_mavg_tbl %>% 
  tail(1) %>% 
  mutate(flag = mavg_short > mavg_long) %>% 
  pull(flag)

if(warning_flag){
  str_glue("In reviewing the Portfolio, since the {short_window}-day moving average is above the {long_window}-day moving average, this indicates a positive trend")
} else {
  str_glue("In reviewing the Portfolio, since the {short_window}-day moving average is below the {long_window}-day moving average, this indicates a positive trend")
}


generate_portfolio_commentary <- function(data){
  
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
  
  if(warning_flag){
    str_glue("In reviewing the portfolio, since the {short_window}-day moving average is above the {long_window}-day moving average, this indicates a positive trend")
  } else {
    str_glue("In reviewing the portfolio, since the {short_window}-day moving average is below the {long_window}-day moving average, this indicates a positive trend")
  }
  
  
}

# Adjusting User Inputs to be based on saved results 
# ADDITIONAL: Adjust the saved percentage weights based on the saved results 

user_base_tbl <- tibble(
  user = c("user1", "user2"),
  password = c("pass1", "pass2"), 
  permissions = c("admin", "standard"),
  name = c("User One", "User Two"),
  favourites = list(c("AAL", "DAL", "UAL"), c("MA", "V", "FB")),
  # last_symbol = c("GOOG", "NFLX")
)

tickers <- user_base_tbl %>% filter(user == "user1") %>% pull(favourites) %>% pluck(1)
count <- tickers %>% length()

# Testing to see based on the tickers saved, the default percentage allocation 
weight_calc <- function(count){
  
  if (count > 3){
    allocation <- 100
    w_1 <- allocation / 4
    return(w_1)
  } else if (count > 2){ 
    allocation <- 100
    w_1 <- allocation / 3
    return(w_1)
  } else if (count > 1){
    allocation <- 100
    w_1 <- allocation / 2
    return(w_1)
  } else {100}
}

weight_calc(4)

# Testing errors
"AAPL" %>% get_symbol_from_user_input()
NULL %>% get_symbol_from_user_input()

symbols <- list(c("AAL", "DAL", "UAL"), c("MA", "V", "FB")) %>% pluck(1)
weights <- symbols %>% length() %>% weight_calc()

weights

# 7.0 SAVE SCRIPTS ----

dump(list = c("plot_portfolio_price",
              "plot_portfolio_index",
              "mavg_calculation", 
              "plot_portfolio_index_mavg",
              "generate_portfolio_commentary"),
     file = "00_scripts/portfolio_analysis_functions.R")

