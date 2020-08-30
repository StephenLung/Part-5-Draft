# LIBRARIES ----

# setwd("C:/Users/steph/Dropbox/Business University Science/Personal Portfolio/Part-5-Draft")
library(pacman)

# Shiny
library(flexdashboard)
library(shiny)
library(shinyjs)
library(shinyWidgets)
library(shinythemes)
library(shinyauthr)

# Cloud DB
library(mongolite)
library(jsonlite)
library(config)

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

p_load(
  flexdashboard,
  shiny,
  shinyjs,
  shinyWidgets,
  shinythemes,
  shinyauthr,
  
  # Cloud DB
  mongolite,
  jsonlite,
  config,
  
  # Core
  tidyquant,
  tidyverse,
  timetk,
  tidyr,
  tibble,
  dplyr,
  furrr,
  purrr,
  glue,
  forcats,
  stringr,
  rlang,
  
  # Text
  tidytext,    # Tidy text mining
  textdata,    # Needed for AFINN
  
  # Visualizations
  plotly,
  highcharter,
  correlationfunnel,
  ggwordcloud, # Extension for wordclouds
  
  # Interactive Maps
  tmaptools,
  leaflet, 
  
  # Twitter API
  rtweet, 
  
  # Modeling
  parsnip,
  xgboost,
  glmnet,
  
  modeltime,
   timetk,
   tidymodels,
   tidyverse,
   lubridate
  
)

# 
# install.packages("remotes")
# remotes::install_github("paulc91/shinyauthr")

p_load(devtools)
require(devtools)
remotes::install_github(repo = "paulc91/shinyauthr")

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


# 1.1 STOCK DATA ----
# Raw code
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

# Function format 

plot_portfolio_price <- function(data, ggplot = TRUE){
  # Takes portfolio data and plots individual prices on the chart
  
  p <- data %>% 
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
  
  if(ggplot){
    ggplotly(p, tooltip = "text") %>% 
      layout(xaxis = list(
        rangeslider = list(type = "date")
      ))
  }
  
}

portfolio_price_data %>% 
  plot_portfolio_price()

# 2.0 CREATE PORTFOLIO DATA ----

# Setup portfolio table
portfolio_data_tbl <- multi_asset_price_portfolio(symbols, end, start, wts_tbl) %>% 
  multi_asset_return_portfolio(period = "monthly") %>% 
  wealth_index(wts_tbl = wts_tbl, name_portfolio = "test portfolio")

portfolio_data_tbl %>% 
  portfolio_mavg_calculation() %>% 
  tail()

portfolio_data_tbl %>% 
  portfolio_mavg_calculation() %>% 
  generate_portfolio_commentary()




# Apply log transformation
# apply log and exp to prove the change
portfolio_data_tbl <- portfolio_data_tbl %>% 
  mutate(log_investment = log(investment.growth)) %>% 
  mutate(exp_investment = exp(log_investment))

log_investment <- portfolio_data_tbl$log_investment
hist(log_investment)


# Raw ggplot code
p <- portfolio_data_tbl %>% 
  mutate(label_text = str_glue("Date: {date}
                               Investment: {scales::dollar(investment.growth)}
                               Growth %: {scales::percent(portfolio.wealthindex-1, accuracy = 0.01)}")) %>% 
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

ggplotly(p, tooltip = "text")

# 2.1 PORTFOLIO DATA
plot_portfolio_index <- function(data, ggplot = TRUE){
  # Takes portfolio index and plots the graph
  p <- data %>% 
    mutate(label_text = str_glue("Date: {date}
                               Investment: {scales::dollar(investment.growth)}
                               Growth %: {scales::percent(portfolio.wealthindex-1, accuracy = 0.01)}")) %>% 
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

# 3.0 ModelTime package ----
install.packages("pacman")
library(pacman)
p_load(modeltime,
       timetk,
       tidymodels,
       tidyverse,
       lubridate)

# plotting stocks
portfolio_price_data %>% 
  group_by(symbol) %>% 
  plot_time_series(date, adjusted,
                   .facet_ncol = 2, # 2-column layout
                   .interactive = TRUE) # interactivity

# plotting portfolio
portfolio_data_tbl %>% 
  plot_time_series(date, investment.growth)

# data cleaning
data_prepared_tbl <- portfolio_data_tbl %>% 
  mutate(value = ifelse(investment.growth == 0, NA, investment.growth)) %>% #Set 0 values as NA
  mutate(value = ts_impute_vec(value, period = 12)) %>% #if there are NAs,it will use a linear interpolation with seasonality
  select(date, value) %>% 
  as_tibble()

# 4.0 MODELLING ----
# turning the last year into a testing dataset
splits <- time_series_split(data_prepared_tbl, assess = "1 year",
                            cumulative = TRUE) # enables all previous data to be used as training

# visualize the split of train and test
splits %>%
  tk_time_series_cv_plan() %>%
  plot_time_series_cv_plan(date, value) 

# * ARIMA ----
model_fit_arima <- arima_reg() %>% #informs parsnip to run auto-arima under forecast package
  set_engine("auto_arima") %>% #connects to the forecast algorithm
  fit(
    value ~ date,  # specify formula value as a function of date
    data = training(splits) #provides the training dataset
  ) # fit 

# * LINEAR REGRESSION ----
model_fit_lm <- linear_reg() %>% #from the parsnip library
  set_engine("lm") %>% #set it to the lm engine 
  fit(
    value ~ as.numeric(date) + month(date, label = TRUE), # few features are created
    data = training(splits)
  )

# * LINEAR REGRESSION - NO TREND ----
model_fit_lm_no_trend <- linear_reg() %>%
  set_engine("lm") %>%
  fit(
    value ~ month(date, label = TRUE), # no numeric here
    data = training(splits)
  )

# * PROPHET ----
model_fit_prophet <- prophet_reg() %>% #from the modeltime library
  set_engine("prophet") %>%
  fit(
    value ~ date, 
    data = training(splits)
  )

# * RANDOM FOREST ----
model_fit_rf <- rand_forest(mode = "regression") %>%
  set_engine("randomForest") %>%
  fit(
    value ~ as.numeric(date) + month(date, label = TRUE), 
    data = training(splits)
  )

# * XGBOOST ----
model_fit_xgboost <- boost_tree(mode = "regression") %>%
  set_engine("xgboost") %>%
  fit(
    value ~ as.numeric(date) + month(date, label = TRUE), 
    data = training(splits)
  )

# * SVM - Polynomial ----
model_fit_svm_poly <- svm_poly(mode = "regression") %>%
  set_engine("kernlab") %>%
  fit(
    value ~ as.numeric(date) + month(date, label = TRUE), 
    data = training(splits)
  )

# * SVM - RBF ----
model_fit_svm_rbf <- svm_rbf(mode = "regression") %>%
  set_engine("kernlab") %>%
  fit(
    value ~ as.numeric(date) + month(date, label = TRUE), 
    data = training(splits)
  )

# * PROPHET BOOST ----
model_fit_prophet_boost <- prophet_boost() %>% #using xgboost on the residuals of prohpet model
  set_engine("prophet_xgboost") %>%
  fit(
    value ~ date + as.numeric(date) + month(date, label = TRUE),  #date is sent to prophet algo, the rest is in xgboost
    data = training(splits)
  )

# * ARIMA BOOST ----
model_fit_arima_boost <- arima_boost() %>%
  set_engine("auto_arima_xgboost") %>%
  fit(
    value ~ date + as.numeric(date) + month(date, label = TRUE), 
    data = training(splits)
  )

# 5.0 MODELTIME FORECAST WORKFLOW ----

# * Modeltime Table ----
model_tbl <- modeltime_table( 
  model_fit_arima,
  model_fit_lm,
  model_fit_lm_no_trend,
  model_fit_prophet,
  model_fit_rf,
  model_fit_xgboost,
  model_fit_svm_poly,
  model_fit_svm_rbf,
  model_fit_prophet_boost,
  model_fit_arima_boost
) # stores it as a tibble and assigns with model id and description

# * Calibrate ----
calibration_tbl <- model_tbl %>%
  modeltime_calibrate(testing(splits))

calibration_tbl %>%
  modeltime_accuracy() %>% #calculates the accuracy of the 10 models
  table_modeltime_accuracy(resizable = TRUE, bordered = TRUE) #provides an interactive table 

calibration_tbl %>%
  modeltime_forecast(
    new_data = testing(splits), 
    actual_data = data_prepared_tbl,
    conf_interval = 0.80
  ) %>%
  plot_modeltime_forecast(.legend_show = TRUE, 
                          .legend_max_width = 50) #produce a forecast of each of the model

# * Refit ----
refit_tbl <- calibration_tbl %>%
  modeltime_refit(data = data_prepared_tbl)  # refitting the models to the full dataset

forecast_tbl <- refit_tbl %>%
  modeltime_forecast(
    h = "1 year", #new time horizon
    actual_data = data_prepared_tbl,
    conf_interval = 0.80
  ) 

forecast_tbl %>%
  plot_modeltime_forecast(.interactive = TRUE)

refit_tbl %>%
  modeltime_accuracy() %>% #calculates the accuracy of the 10 models
  table_modeltime_accuracy(resizable = TRUE, bordered = TRUE) 




# 6.0 ModelTime package (LOG TRANSFORMATION) ---- 

# plotting portfolio
portfolio_data_tbl %>% 
  plot_time_series(date, log_investment)

# data cleaning and logging portfolio
data_prepared_tbl <- portfolio_data_tbl %>% 
  mutate(value = ifelse(log_investment == 0, NA, log_investment)) %>% #Set 0 values as NA
  mutate(value = ts_impute_vec(value, period = 12)) %>% #if there are NAs,it will use a linear interpolation with seasonality
  select(date, value) %>% 
  as_tibble()

data_prepared_pre_log_tbl <- portfolio_data_tbl %>% 
  mutate(value = investment.growth) %>% 
  select(date, value)

# 6.1 MODELLING ----
# turning the last year into a testing dataset
splits <- time_series_split(data_prepared_tbl, assess = "1 year",
                            cumulative = TRUE) # enables all previous data to be used as training

# visualize the split of train and test
splits %>%
  tk_time_series_cv_plan() %>%
  plot_time_series_cv_plan(date, value) 

# * ARIMA ----
model_fit_arima <- arima_reg() %>% #informs parsnip to run auto-arima under forecast package
  set_engine("auto_arima") %>% #connects to the forecast algorithm
  fit(
    value ~ date,  # specify formula value as a function of date
    data = training(splits) #provides the training dataset
  ) # fit 

# * LINEAR REGRESSION ----
model_fit_lm <- linear_reg() %>% #from the parsnip library
  set_engine("lm") %>% #set it to the lm engine 
  fit(
    value ~ as.numeric(date) + month(date, label = TRUE), # few features are created
    data = training(splits)
  )

# * LINEAR REGRESSION - NO TREND ----
model_fit_lm_no_trend <- linear_reg() %>%
  set_engine("lm") %>%
  fit(
    value ~ month(date, label = TRUE), # no numeric here
    data = training(splits)
  )

# * PROPHET ----
model_fit_prophet <- prophet_reg() %>% #from the modeltime library
  set_engine("prophet") %>%
  fit(
    value ~ date, 
    data = training(splits)
  )

# * RANDOM FOREST ----
model_fit_rf <- rand_forest(mode = "regression") %>%
  set_engine("randomForest") %>%
  fit(
    value ~ as.numeric(date) + month(date, label = TRUE), 
    data = training(splits)
  )

# * XGBOOST ----
model_fit_xgboost <- boost_tree(mode = "regression") %>%
  set_engine("xgboost") %>%
  fit(
    value ~ as.numeric(date) + month(date, label = TRUE), 
    data = training(splits)
  )

# * SVM - Polynomial ----
model_fit_svm_poly <- svm_poly(mode = "regression") %>%
  set_engine("kernlab") %>%
  fit(
    value ~ as.numeric(date) + month(date, label = TRUE), 
    data = training(splits)
  )

# * SVM - RBF ----
model_fit_svm_rbf <- svm_rbf(mode = "regression") %>%
  set_engine("kernlab") %>%
  fit(
    value ~ as.numeric(date) + month(date, label = TRUE), 
    data = training(splits)
  )

# * PROPHET BOOST ----
model_fit_prophet_boost <- prophet_boost() %>% #using xgboost on the residuals of prohpet model
  set_engine("prophet_xgboost") %>%
  fit(
    value ~ date + as.numeric(date) + month(date, label = TRUE),  #date is sent to prophet algo, the rest is in xgboost
    data = training(splits)
  )

# * ARIMA BOOST ----
model_fit_arima_boost <- arima_boost() %>%
  set_engine("auto_arima_xgboost") %>%
  fit(
    value ~ date + as.numeric(date) + month(date, label = TRUE), 
    data = training(splits)
  )

# 6.2 MODELTIME FORECAST WORKFLOW ----

# * Modeltime Table ----
model_tbl <- modeltime_table( 
  model_fit_arima,
  model_fit_lm,
  model_fit_lm_no_trend,
  model_fit_prophet,
  model_fit_rf,
  model_fit_xgboost,
  model_fit_svm_poly,
  model_fit_svm_rbf,
  model_fit_prophet_boost,
  model_fit_arima_boost
) # stores it as a tibble and assigns with model id and description

# * Calibrate ----
calibration_tbl <- model_tbl %>%
  modeltime_calibrate(testing(splits))

calibration_tbl %>% 
  pull(.calibration_data) %>%
  select_if(is.numeric)
  map(function(x) exp(x))

map(calibration_tbl %>% pull(.calibration_data),
    3)

exponent <- function(x) {
  x %>% 
    select_if(is.numeric())
}


### 
calibration_tbl %>%
  modeltime_accuracy() %>% #calculates the accuracy of the 10 models
  table_modeltime_accuracy(resizable = TRUE, bordered = TRUE) #provides an interactive table 

calibration_tbl %>%
  modeltime_forecast(
    new_data = testing(splits) %>% mutate(value = exp(value)), # apply exp()
    actual_data = data_prepared_tbl, 
    conf_interval = 0.80
  ) %>%
  mutate(.value = exp(.value),
         .conf_lo = exp(.conf_lo),
         .conf_hi = exp(.conf_hi)) %>% 
  plot_modeltime_forecast(.legend_show = TRUE, 
                          .legend_max_width = 50) #produce a forecast of each of the model

# * Refit ----
refit_tbl <- calibration_tbl %>%
  modeltime_refit(data = data_prepared_tbl)  # refitting the models to the full dataset

forecast_tbl <- refit_tbl %>%
  modeltime_forecast(
    h = "1 year", #new time horizon
    actual_data = data_prepared_tbl,
    conf_interval = 0.80
  ) %>% 
  mutate(.value = exp(.value),
         .conf_lo = exp(.conf_lo),
         .conf_hi = exp(.conf_hi))

forecast_tbl %>%
  plot_modeltime_forecast(.interactive = TRUE,
                          .title = "Forecast Plot with log transformation")

refit_tbl %>%
  modeltime_accuracy() %>% #calculates the accuracy of the 10 models
  table_modeltime_accuracy(resizable = TRUE, bordered = TRUE) 

