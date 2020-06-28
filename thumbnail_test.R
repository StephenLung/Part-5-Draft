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

# source("00_scripts/stock_analysis_functions.R")
# source("00_scripts/portfolio_analysis_functions.R")
# source("00_prior_script/wealth_index.R")
# source("00_scripts/info_card.R")
# source("00_scripts/panel_card.R")
# source("00_scripts/generate_favourite_cards.R")
# source("00_scripts/geocode_for_free.R") #geocoding for locations

stock_list_tbl <- get_stock_list("SP500")
current_user_favourites <- c("AAPL", "MSFT", "NFLX", "AMZN") # needed for the default setting
token <- read_rds("../my_twitter_token.rds")

# USER DATA ----
current_user_favorites <- c("AAPL", "GOOG", "NFLX")
ui <- tagList(
  
  
  #CSS to change contextual themes
  tags$head(
    tags$link(rel = "stylesheet",
              type = "text/css",
              href = shinytheme("cyborg")), # Adds a default theme
    tags$link(rel = "stylesheet",
              type = "text/css",
              href = "styles.css") # Adds the styles
  ),
  
  # JS ----
  shinyjs::useShinyjs(),
  
  # Website ----
  uiOutput(outputId = "website")
  
)

server <- function(input, output, session) {
  
  # 5.0 RENDER WEBSITE ----
  
  output$website <- renderUI({
    
    # req(credentials()$user_auth) #web page is displayed when user authentication is correct
    
    navbarPage(
      title = "Portfolio Analytics Prediction App",
      inverse = TRUE,
      collapsible = TRUE,
      theme = shinytheme("darkly"), # Sets up the website with darkly theme
      id = "inNavset",
      
      # In navbarpage, this header will show the username in every tab 
      header = div( 
        class = "pull-right",
        style = "padding-right: 20px;"
        # p("Welcome, ", reactive_values$user_name)
      ), 
      
      tabPanel(
        title = "Home",
        value = "Home",
        
        # THUMBNAILS ----
        div(
          class = "container",
          id = "home-page",
          # HEADER ----
          div(
            class = "container",
            id = "header",
            h1(class = "page-header",
               "Portfolio Analytics Prediction App",
               tags$small("In Shiny")),
            p(class = "lead",
              "Learning to build a portfolio analytics app for friends and family",
              code("in Shiny")),
          ),
          
          # 5.1.0 THUMBNAILS ----
          div(
            class = "container",
            id = "selection_screen",
            h2("Welcome to the App"),
            fluidRow(
              column(
                width = 4,
                div(
                  class = "thumbnail text-center",
                  img(class = "thumbnail img-responsive",
                      src = "stock-2.jpg"),
                  h3("Stock Analysis"),
                  p("Placeholder"),
                  # a(class = "btn btn-primary btn-md", href = "section_1", "Learn More"),
                  # action button to link to next tabPanel
                  actionButton(inputId = "action_section_1", label = "Learn More",
                               class = "btn btn-primary btn-md")
                )
              ),
              column(
                width = 4,
                div(
                  class = "thumbnail text-center",
                  img(class = "thumbnail img-responsive",
                      src = "portfolio-2.jpg"),
                  h3("Portfolio Analysis"),
                  p("Placeholder"),
                  # a(class = "btn btn-success btn-md", href = "#", "Learn More"),
                  actionButton(inputId = "action_section_2", label = "Learn More",
                               class = "btn btn-primary btn-md")
                )
              ),
              column(
                width = 4,
                div(
                  class = "thumbnail text-center",
                  img(class = "thumbnail img-responsive",
                      src = "stephen-pic.jpg"),
                  h3("About the author"),
                  p("Placeholder"),
                  # a(class = "btn btn-danger btn-md", href = "#", "Learn More"),
                  actionButton(inputId = "action_section_3", label = "Learn More",
                               class = "btn btn-primary btn-md")
                )
              )
            )
          )
          
          
        ),
        
        
      )
  
  )
  })
}

shinyApp(ui, server)