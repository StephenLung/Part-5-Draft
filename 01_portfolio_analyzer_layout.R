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


# UI ----
ui <- fluidPage(
  title = "Portfolio Analytics Prediction App",
  
  # 1.0 HEADER ----
  div(
    h1("Portfolio Analytics Prediction App"),
    p("This is the beta testing of portfolio analytics project in Shiny")
  ),
  
  # 2.0 THREE WELL PANEL APPLICATION UI ----
  div(
    div(
      class = "col-sm-4",
      fluidRow(
        column(
          id = "1",
          class = "well",
          width = 6,
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
                      ))
          
        ),
        column(
          id = "2",
          class = "well",
          width = 6,
          numericInput("w1", h5("Portf. %"), 25,
                       min = 1, max = 100),
        
        numericInput("w2", h5("Portf. %"), 25,
                     min = 1, max = 100),
        
        numericInput("w3", h5("Portf. %"), 25,
                     min = 1, max = 100),
        
        numericInput("w4", h5("Portf. %"), 25,
                     min = 1, max = 100)
        )
      ),
      fluidRow(
        column(
          id = "3",
          class = "well",
          width = 12,
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
                                   lib = "font-awesome")),
          hr(),
          sliderInput(inputId = "mavg_short", label = "Short Moving Average", 
                      min = 5, max = 40, value = 20),
          
          sliderInput(inputId = "mavg_long", label = "Long Moving Average", 
                      min = 50, max = 120, value = 50)    
          
        )
      )
    ),
    div(
      class = "col-sm-8",
                 div(
                   div(h4("Stock Selected is...")),
                   div(
                     # tableOutput(outputId = "portfolio_price_data_tbl"),
                     plotlyOutput(outputId = "portfolio_price_data_tbl")

                   )
                 ),
                 div(
                   div(h4("Stock Returns of selected is....")),
                   div(
                     plotlyOutput(outputId = "portfolio_index_mavg_data_tbl")

                     # portfolio_return_data_tbl %>%
                     #   plot_portfolio_index()
                   )
                 )
    )
    
    
  ),
  
  # 3.0 PORTFOLIO COMMENTARY ----
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
  
  # Stock symbols  ----
  # Warning user input requires parsing to the ticker symbol
  symbols <- eventReactive(input$submit,
                valueExpr = {
    c(input$stock_1 %>% get_symbol_from_user_input(),
      input$stock_2 %>% get_symbol_from_user_input(),
      input$stock_3 %>% get_symbol_from_user_input(),
      input$stock_4 %>% get_symbol_from_user_input())
  }, ignoreNULL = FALSE)
  
  # Ticker weights ----
  weight <- eventReactive(input$submit,
                          valueExpr = {
                            c(input$w1/100,
                              input$w2/100,
                              input$w3/100,
                              input$w4/100)
                          }, ignoreNULL = FALSE)
  
  # Combined tbl with stocks and weights ----
  wts_tbl <- reactive({
    tibble(
      symbols(), weight()
    )})
  output$wts_tbl <- renderTable(wts_tbl())
  
  # End Date
  end <- eventReactive(input$submit,
                       valueExpr = {
                         input$end_date
                       }, ignoreNULL = FALSE)
  
  # Start Date
  start <- eventReactive(input$submit,
                         valueExpr = {
                           input$start_date
                         }, ignoreNULL = FALSE)
  
  
  # Prices of each ticker Plot ----
  portfolio_price_data_tbl <- reactive({
    multi_asset_price_portfolio(symbols = symbols(), 
                                end = end(), 
                                start = start(), 
                                wts_tbl = wts_tbl()) %>% 
      plot_portfolio_price()
  })
  output$portfolio_price_data_tbl <- renderPlotly(portfolio_price_data_tbl())
  
  # Portfolio investment index Plot ----
  portfolio_index_data_tbl <- reactive({
    multi_asset_price_portfolio(symbols = symbols(),
                                end = end(),
                                start = start(),
                                wts_tbl = wts_tbl()) %>%
      multi_asset_return_portfolio(period = "monthly") %>%
      wealth_index(wts_tbl = wts_tbl(), name_portfolio = "test portfolio"
                  ) %>%
      plot_portfolio_index()
  })
  output$portfolio_index_data_tbl <- renderPlotly(portfolio_index_data_tbl())
  
  # Portfolio investment index with MAVG plot ----
  portfolio_index_mavg_data_tbl <- reactive({
    multi_asset_price_portfolio(symbols = symbols(),
                                end = end(),
                                start = start(),
                                wts_tbl = wts_tbl()) %>%
      multi_asset_return_portfolio(period = "monthly") %>%
      wealth_index(wts_tbl = wts_tbl(), name_portfolio = "test portfolio") %>%
      mavg_calculation(mavg_short = input$mavg_short, 
                       mavg_long = input$mavg_long) %>% 
      plot_portfolio_index_mavg()
    
    
  })
  output$portfolio_index_mavg_data_tbl <- renderPlotly(portfolio_index_mavg_data_tbl())

  
}


# RUN APP ----
shinyApp(ui = ui, server = server)

