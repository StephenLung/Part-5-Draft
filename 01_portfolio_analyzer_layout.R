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
library(rlang)

# Visualizations
library(plotly)
library(highcharter)
library(correlationfunnel)

# Modeling
library(parsnip)
library(xgboost)
library(glmnet)

source("00_scripts/stock_analysis_functions.R")
source("00_scripts/portfolio_analysis_functions.R")
source("00_prior_script/wealth_index.R")

# Stock analysis ----
# stock_list_tbl <- get_stock_list("SP500")
# user_input <- "AAPL, Apple Inc."
# stock_data_tbl <- get_symbol_from_user_input(user_input) %>% 
#   get_stock_prices()


portfolio_return_data_tbl <- multi_asset_price_portfolio(symbols, end, start, wts_tbl) %>%
  multi_asset_return_portfolio(period = "monthly") %>%
  wealth_index(wts_tbl = wts_tbl, name_portfolio = "test portfolio")

multi_asset_price_portfolio(symbols, end, start, wts_tbl) %>% 
  plot_portfolio_price()

# UI ----
ui <- fluidPage(
  title = "Portfolio Analytics Prediction App",
  
  # 1.0 HEADER ----
  div(
    h1("Portfolio Analytics Prediction App"),
    p("This is the beta testing of portfolio analytics project in Shiny")
  ),
  
  
  # 2.1 TWO WELL PANEL APPLICATION UI ----
  div(
    fluidRow(
      column(width = 2, 
             wellPanel(
               pickerInput(inputId = "stock_1",
                           label = h5("Stock 1"),
                           choices = stock_list_tbl$label,
                           multiple = FALSE,
                           selected = stock_list_tbl %>% filter(label %>% str_detect("AAPL")) %>% pull(label),
                           options = pickerOptions(
                             actionsBox = FALSE,
                             liveSearch = TRUE,
                             size = 10
                           )),
               pickerInput(inputId = "stock_2",
                           label = h5("Stock 2"),
                           choices = stock_list_tbl$label,
                           multiple = FALSE,
                           selected = stock_list_tbl %>% filter(label %>% str_detect("MSFT")) %>% pull(label),
                           options = pickerOptions(
                             actionsBox = FALSE,
                             liveSearch = TRUE,
                             size = 10
                           )),
               pickerInput(inputId = "stock_3",
                           label = h5("Stock 3"),
                           choices = stock_list_tbl$label,
                           multiple = FALSE,
                           selected = stock_list_tbl %>% filter(label %>% str_detect("NFLX")) %>% pull(label),
                           options = pickerOptions(
                             actionsBox = FALSE,
                             liveSearch = TRUE,
                             size = 10
                           )),
               pickerInput(inputId = "stock_4",
                           label = h5("Stock 4"),
                           choices = stock_list_tbl$label,
                           multiple = FALSE,
                           selected = stock_list_tbl %>% filter(label %>% str_detect("AMZN")) %>% pull(label),
                           options = pickerOptions(
                             actionsBox = FALSE,
                             liveSearch = TRUE,
                             size = 10
                           )),
               dateInput("start_date",
                         h4("Starting Date"),
                         "2006-02-01",
                         format = "yyyy-mm-dd"),
               dateInput("end_date",
                         h4("End Date"),
                         today(),
                         format = "yyyy-mm-dd"),
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
      column(width = 2, 
             wellPanel(
               numericInput("w1", h5("Portf. %"), 25,
                            min = 1, max = 100),
               
               numericInput("w2", h5("Portf. %"), 25,
                            min = 1, max = 100),
               
               numericInput("w3", h5("Portf. %"), 25,
                            min = 1, max = 100),
               
               numericInput("w4", h5("Portf. %"), 25,
                            min = 1, max = 100),
               
             )),
      column(width = 8,
             div(
               div(h4("Stock Selected is...")),
               div(
                 
                 tableOutput(outputId = "wts_tbl"),
                 # verbatimTextOutput(outputId = "portfolio_price_data_tbl")
                 plotlyOutput(outputId = "portfolio_price_data_tbl"),
                 plotlyOutput(outputId = "portfolio_index_data_tbl")
                 # plotlyOutput(outputId = "time_plot_tbl")
                   
                 # portfolio_price_data_tbl %>% 
                 #   plot_portfolio_price()
                 
               )
             ),
             div(
               div(h4("Stock Returns of selected is....")),
               div(
                 portfolio_return_data_tbl %>% 
                   plot_portfolio_index()
               )
             ))
    )
  ),
  
  # 2.0 APPLICATION UI ----
  # div(
  #   column(width = 4,
  #          wellPanel(
  #            pickerInput(inputId = "stock_1",
  #                        label = "Stock 1",
  #                        choices = stock_list_tbl$label,
  #                        multiple = FALSE,
  #                        selected = stock_list_tbl %>% filter(label %>% str_detect("AAPL")) %>% pull(label),
  #                        options = pickerOptions(
  #                          actionsBox = FALSE,
  #                          liveSearch = TRUE,
  #                          size = 10
  #                        )),
  #            pickerInput(inputId = "stock_2",
  #                        label = "Stock 2",
  #                        choices = stock_list_tbl$label,
  #                        multiple = FALSE,
  #                        selected = stock_list_tbl %>% filter(label %>% str_detect("MSFT")) %>% pull(label),
  #                        options = pickerOptions(
  #                          actionsBox = FALSE,
  #                          liveSearch = TRUE,
  #                          size = 10
  #                        )),
  #            pickerInput(inputId = "stock_3",
  #                        label = "Stock 3",
  #                        choices = stock_list_tbl$label,
  #                        multiple = FALSE,
  #                        selected = stock_list_tbl %>% filter(label %>% str_detect("NFLX")) %>% pull(label),
  #                        options = pickerOptions(
  #                          actionsBox = FALSE,
  #                          liveSearch = TRUE,
  #                          size = 10
  #                        )),
  #            
  #            actionButton(inputId = "submit",
  #                         label = "Submit",
  #                         icon = icon("piggy-bank",
  #                                     lib = "font-awesome")),
  #            actionButton(inputId = "reset",
  #                         label = "Reset",
  #                         icon = icon("sync", 
  #                                     lib = "font-awesome"))
  #          )
  #          ),
  #   column(width = 8,
  #          div(
  #            div(h4("Stock Selected is...")),
  #            div(
  #              stock_data_tbl %>% 
  #                plot_data_prices()
  #              
  #            )
  #          ),
  #          div(
  #            div(h4("Stock Returns of selected is....")),
  #            div(
  #              stock_data_tbl %>% 
  #                get_stock_returns() %>% 
  #                plot_data_returns()
  #            )
  #          ))
  # ),
  
  
  # 3.0 ANALYST COMMENTARY ----
  div(
    column(width = 12,
           div(
             div(h4("Analyst Commentary")),
             div(
               # stock_data_tbl %>% 
               #   generate_commentary(user_input = user_input)
             )
           )
           
           )
  )
)

# SERVER ----
server <- function(input, output, session){
  
  # STOCK SYMBOLS ----
  symbols <- eventReactive(input$submit,
                valueExpr = {
    c(input$stock_1 %>% get_symbol_from_user_input(),
      input$stock_2 %>% get_symbol_from_user_input(),
      input$stock_3 %>% get_symbol_from_user_input(),
      input$stock_4 %>% get_symbol_from_user_input())
  }, ignoreNULL = FALSE)
  
  weight <- eventReactive(input$submit,
                          valueExpr = {
                            c(input$w1/100,
                              input$w2/100,
                              input$w3/100,
                              input$w4/100)
                          }, ignoreNULL = FALSE)
  
  wts_tbl <- reactive({
    tibble(
      symbols(), weight()
    )
  })
  output$wts_tbl <- renderTable(wts_tbl())
  
  end <- eventReactive(input$submit,
                       valueExpr = {
                         input$end_date
                       }, ignoreNULL = FALSE)
  
  start <- eventReactive(input$submit,
                         valueExpr = {
                           input$start_date
                         }, ignoreNULL = FALSE)
  
  portfolio_price_data_tbl <- reactive({
    multi_asset_price_portfolio(symbols = symbols(), 
                                end = end(), 
                                start = start(), 
                                wts_tbl = wts_tbl()) %>% 
      plot_portfolio_price()
  })
  output$portfolio_price_data_tbl <- renderPlotly(portfolio_price_data_tbl())
  
  
  time_plot_tbl <- eventReactive(input$submit, {
    
    symbols <- c(input$stock1, 
                 input$stock2,
                 input$stock3,
                 input$stock4)
    
    end <- input$end_date
    start <- input$start_date
    
    w <- c(input$w1/100,
           input$w2/100,
           input$w3/100,
           input$w4/100)
    
    wts_tbl <- tibble(symbols, w)
    
    multi_asset_price_portfolio(symbols, end, start, wts_tbl) %>% 
      plot_portfolio_price()
}, ignoreNULL = FALSE)
  
  output$time_plot_tbl <- renderPlotly(time_plot_tbl())
  
  portfolio_index_data_tbl <- reactive({
    multi_asset_price_portfolio(symbols = symbols(), 
                                end = end(), 
                                start = start(), 
                                wts_tbl = wts_tbl()) %>% 
      multi_asset_return_portfolio(period = "monthly") %>% 
      wealth_index(wts_tbl = wts_tbl(), name_portfolio = "test portfolio") %>% 
      plot_portfolio_index()
  })
  
  output$portfolio_index_data_tbl <- renderPlotly(portfolio_index_data_tbl())
  
  
}


# RUN APP ----
shinyApp(ui = ui, server = server)