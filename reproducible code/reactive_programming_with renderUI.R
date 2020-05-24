# LIBRARIES ----

# setwd("C:/Users/steph/Dropbox/Business University Science/Personal Portfolio/Part-5-Draft")
library(pacman)

# Shiny
library(flexdashboard)
library(shiny)
library(shinyjs)
library(shinyWidgets)
library(shinythemes)

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

stock_list_tbl <- tq_index("SP500")
token <- read_rds("../my_twitter_token.rds")

ui <- fluidPage(
  
  pickerInput(inputId = "stock_1",
              label = h5("Stock 1"),
              choices = stock_list_tbl$company,
              multiple = FALSE,
              selected = stock_list_tbl %>% filter(symbol %>% str_detect("AAPL")) %>% pull(company),
              options = pickerOptions(
                actionsBox = FALSE,
                liveSearch = TRUE,
                size = 10
              )),
  uiOutput(outputId = "sentiment_search"),
  textOutput(outputId = "sentiment")
  
)

server <- function(input, output, session) {
  
  output$sentiment_search <- renderUI({
    input_stock <- input$stock_1 %>% str_split(pattern = " ") %>% pluck(1, 1)
    shiny::textInput(inputId = "query_test", label = "Topic / Hashtag", value = input_stock)
  })
  
  
  rv <- reactiveValues()
  
  rv$data <-  search_tweets(
    q           = input$query_test, #PROBLEM: LENGTH IS NOT ONE
    n           = input$n_tweets,
    include_rts = FALSE,
    lang        = "en",
    token       = token
  )
  
}

shinyApp(ui, server)