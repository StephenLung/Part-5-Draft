# LIBRARIES ----

# setwd("C:/Users/steph/Dropbox/Business University Science/Personal Portfolio/WIP/All_Seasons_Fund_WIP")
library(flexdashboard)
library(pacman)

library(flexdashboard)
library(shiny)
library(shinyjs)
library(shinyWidgets)

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

# Visualizations
library(plotly)
library(highcharter)
library(correlationfunnel)

# Modeling
library(parsnip)
library(xgboost)
library(glmnet)


stock_list_tbl <- get_stock_list("SP500")
user_input <- "AAPL, Apple Inc."
stock_data_tbl <- get_symbol_from_user_input(user_input) %>% 
  get_stock_prices()

# UI ----
ui <- fluidPage(
  title = "Portfolio Analytics Prediction App",
  
  # 1.0 HEADER ----
  div(
    h1("Portfolio Analytics Prediction App"),
    p("This is the beta testing of portfolio analytics project in Shiny")
  ),
  
  # 2.0 APPLICATION UI ----
  div(
    column(width = 4,
           wellPanel(
             pickerInput(inputId = "stock_1",
                         label = "Stock 1",
                         choices = stock_list_tbl$label,
                         multiple = FALSE,
                         selected = stock_list_tbl %>% filter(label %>% str_detect("AAPL")) %>% pull(label),
                         options = pickerOptions(
                           actionsBox = FALSE,
                           liveSearch = TRUE,
                           size = 10
                         )),
             actionButton(inputId = "submit",
                          label = "Submit",
                          icon = icon("piggy-bank",
                                      lib = "font-awesome")),
             actionButton(inputId = "reset",
                          label = "Reset",
                          icon = icon("sync", 
                                      lib = "font-awesome"))
           )
           ),
    column(width = 8,
           div(
             div(h4("Stock Selected is...")),
             div(
               stock_data_tbl %>% 
                 plot_data_prices()
               
             )
           ),
           div(
             div(h4("Stock Returns of selected is....")),
             div(
               stock_data_tbl %>% 
                 get_stock_returns() %>% 
                 plot_data_returns()
             )
           ))
  ),
  
  
  # 3.0 ANALYST COMMENTARY ----
  div(
    column(width = 12)
  )
)



# SERVER ----
server <- function(input, output, session){
  
}


# RUN APP ----
shinyApp(ui = ui, server = server)