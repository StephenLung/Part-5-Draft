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

source("00_scripts/stock_analysis_functions.R")
source("00_scripts/portfolio_analysis_functions.R")
source("00_prior_script/wealth_index.R")
source("00_scripts/info_card.R")
source("00_scripts/generate_favourite_cards.R")
source("00_scripts/geocode_for_free.R") #geocoding for locations

stock_list_tbl <- get_stock_list("SP500")
current_user_favourites <- c("AAPL", "MSFT", "NFLX", "AMZN") # needed for the default setting
token <- read_rds("../my_twitter_token.rds")

# USER DATA ----
current_user_favorites <- c("AAPL", "GOOG", "NFLX")


# UI ----
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
  
  # User Login ----
  # Setup login UI using shinyauthr settings
  verbatimTextOutput(outputId = "creds"),
  shinyauthr::loginUI(
    id = "login",
    title = tagList(h2(class = "text-center", "Stock Analyzer", tags$small("in Shiny App") %>% br()), 
                    p(class = "text-center","Please Log In")),
    login_title = "Enter"
  ),
  
  # Website ----
  uiOutput(outputId = "website")
  
)
  
  

# SERVER ----
server <- function(input, output, session){
  
  
  # 0.0 READ DATA ----

  # user_base_tbl <- tibble(
  #   user = c("user1", "user2"),
  #   password = c("pass1", "pass2"), 
  #   permissions = c("admin", "standard"),
  #   name = c("User One", "User Two"),
  #   favourites = list(c("AAL", "DAL", "UAL"), c("MA", "V", "FB")),
  #   # last_symbol = c("GOOG", "NFLX"),
  #   user_settings = list(tibble(mavg_short = 20, mavg_long = 50, start_date = "2018-01-01", end_date = "2020-01-01"),
  #                        tibble(mavg_short = 30, mavg_long = 90, start_date = "2015-01-01", end_date = today()))
  # )
  user_base_tbl <<- read_user_base()
  
  # 0.0 USER LOGIN ----
  

  
  
  # 0.1 Credentials ----
  credentials <- callModule(module = shinyauthr::login,
             id                    = "login", # connec to the UI id
             data                  = user_base_tbl,
             user_col              = user,
             pwd_col               = password,
             log_out               = reactive(logout_init()))
  
  logout_init <- callModule(
    module                         = shinyauthr::logout,
    id                             = "logout", # logout functionality required in the shinyauthr::login module
    active                         = reactive(credentials()$user_auth) # this will be toggled as off when user logs out
  )

  # 0.2 Instantiating User Information ----
  reactive_values <- reactiveValues() # creates a list of reactive values
  
  # Once user authenticates, the row with the credentials is picked up and setup as reactive list
  observe({
    if (credentials()$user_auth){
      
      user_data_tbl <- credentials()$info
      
      reactive_values$permissions <- user_data_tbl$permissions
      reactive_values$user_name <- user_data_tbl$name
      reactive_values$favourites_list <- user_data_tbl$favourites %>% pluck(1)
      # reactive_values$last_symbol <- user_data_tbl$last_symbol 
      reactive_values$user_settings <- user_data_tbl$user_settings 
  }
  })
  
  # Mainly for verbatimTextoutput to view the underlying data
  output$creds <- renderPrint({
    list(reactive_values$permissions,
         reactive_values$user_name,
         reactive_values$favourites_list,
         reactive_values$mavg_short,
         reactive_values$mavg_long,
         reactive_values$start_date,
         reactive_values$end_date,
         credentials(),
         credentials()$info %>% unlist()
         )
         
  })

  
  # 1.0 SETTINGS ----
  
  # 1.1 Toggle to unhide/hide settings ----
  observeEvent(input$settings_toggle, {
    shinyjs::toggle(id = "input_settings", anim = TRUE)
  })
  # 1.2 Button selection to open tabpanel ----
  # Select Stock Analysis
  observeEvent(eventExpr = input$action_section_1,{
    updateNavbarPage(session, "inNavset", selected = "page_1")
  })
  # Select Portfolio Analysis
  observeEvent(eventExpr = input$action_section_2,{
    updateNavbarPage(session, "inNavset", selected = "page_2")
  })
  # Select About the Author
  observeEvent(eventExpr = input$action_section_3,{
    updateNavbarPage(session, "inNavset", selected = "page_3")
  })

  
  # 1.3 Stock symbols  ----
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
  
  # 1.4 Apply & Save Settings ----
  mavg_short <- eventReactive(input$apply_and_save,{
    input$mavg_short
  }, ignoreNULL=FALSE)
  
  mavg_long <- eventReactive(input$apply_and_save,{
    input$mavg_long
  }, ignoreNULL=FALSE)
  
  selected_tab <- eventReactive(input$apply_and_save,{
    if (is.character(input$tab_panel_stock_chart)){
      # Tab already selected
      selected_tab <- input$tab_panel_stock_chart
    } else {
      # Tab panel not built yet
      selected_tab <- "Stock Portfolio Analysis"
    }
    selected_tab
  }, ignoreNULL=FALSE)
  
  # 2.0 FAVOURITES ----
  
  # 2.1 Reactive Values - User Favourites ----

  
  # 2.2 Add Favourites ----
  observeEvent(eventExpr = input$favourites_add, {
    
    reactive_values$favourites_list <- c(reactive_values$favourites_list, 
                                         input$stock_1 %>% get_symbol_from_user_input(),
                                         input$stock_2 %>% get_symbol_from_user_input(),
                                         input$stock_3 %>% get_symbol_from_user_input(),
                                         input$stock_4 %>% get_symbol_from_user_input()) %>% 
      unique()
    
  })
  
  # 2.3 Render Favourite Cards ----
  output$favourite_cards <- renderUI({
    
    if(length(reactive_values$favourites_list) >0){
      generate_favourite_cards(
        favourites_ticker  = reactive_values$favourites_list,
        start              = input$start_date,
        end                = input$end_date,
        mavg_short         = mavg_short(),
        mavg_long          = mavg_long()
      )
      
    }
  })
  
  # 2.4 Delete Favourites ---- 
  observeEvent(eventExpr = input$favourites_clear,{
    
    modalDialog(title = "Clear Favourites", #enables modal pop up message
                size = "m",
                easyClose = TRUE, #allows user to click away from pop up to close
                p(class = "lead",
                  "Are you sure you want to remove favourites?") %>% strong(),
                br(),
                div(
                  selectInput(inputId = "drop_list",
                              label = "Remove Single Favourite",
                              choices = reactive_values$favourites_list %>% sort()
                              ),
                  actionButton(inputId = "remove_single_favourite",
                               label = "Clear Single",
                               class = "btn-warning"),
                  actionButton(inputId = "remove_all_favourite",
                               label = "Clear ALL Favourites",
                               class = "btn-danger")
                ),
                
                
                
                footer = modalButton("Exit") #button at the bottom to exit
                
                ) %>% showModal() #shows the modal on screen
  })
  
  # 2.4.1 Clear Single ----
  observeEvent(eventExpr = input$remove_single_favourite,{
    
    # reactive_values$favourites_list <- reactive_values$favourites_list %>%
    #   .[reactive_values$favourites_list != input$drop_list]
    
    reactive_values$favourites_list <- reactive_values$favourites_list %>%
      data.frame(ticker = .) %>%
      filter(ticker != input$drop_list) %>%
      pull(ticker) %>%
      as.character()
    
    
    updateSelectInput(session = session,
                      inputId = "drop_list",
                      choices = reactive_values$favourites_list %>% sort())
  })
  
  
  # 2.4.2 Clear All ----
  observeEvent(eventExpr = input$remove_all_favourite,{
    reactive_values$favourites_list <- NULL
    updateSelectInput(session = session,
                      inputId = "drop_list",
                      choices = "") # Reflects the new empty list
  })
  
  # 2.5 Show/Hide Favourites ----
  
  observeEvent(input$favourites_toggle,{
    shinyjs::toggle("favourite_cards_section", 
                    anim = TRUE, animType = "fade")
  })
  # shinyjs::onclick(id = "favourites_toggle",{
  #   shinyjs::toggle("favourite_cards_section", 
  #                   anim = TRUE, animType = "fade")
  # })
  
  
  # 3.0 FAVOURITE PLOT ----
  # 3.1 Portfolio: Prices of each ticker Plot ----
  portfolio_price_data_tbl <- reactive({
    multi_asset_price_portfolio(symbols = symbols(), 
                                end = end(), 
                                start = start(), 
                                wts_tbl = wts_tbl()) %>% 
      plot_portfolio_price()
  })
  output$portfolio_price_data_tbl <- renderPlotly(portfolio_price_data_tbl())
  
  # 3.2 Plot Header of stock ticker----
  stock_selection_triggered <- eventReactive(input$submit,{
    str_glue("Stock prices of the following: {get_symbol_from_user_input(input$stock_1)}, 
             {get_symbol_from_user_input(input$stock_2)}, 
             {get_symbol_from_user_input(input$stock_3)}, 
             {get_symbol_from_user_input(input$stock_4)}")
  }, ignoreNULL = FALSE)
  
  output$plot_header <- renderText(
    stock_selection_triggered()
  )
  
  
  
  # Portfolio data tbl
  portfolio_data_mavg_tbl <- reactive({
    multi_asset_price_portfolio(symbols = symbols(),
                                end = end(),
                                start = start(),
                                wts_tbl = wts_tbl()) %>% # grabs list of stocks
      multi_asset_return_portfolio(period = "monthly") %>% # convert prices to returns
      wealth_index(wts_tbl = wts_tbl(), name_portfolio = "test portfolio") %>% # build a wealth index
      portfolio_mavg_calculation(mavg_short = mavg_short(), # calculate moving avg 
                                 mavg_long = mavg_long())
    
  })
  
  # 3.3 Portfolio investment index Plot ----
  portfolio_index_data_tbl <- reactive({
    portfolio_data_mavg_tbl()  %>%
      plot_portfolio_index()
  })
  output$portfolio_index_data_tbl <- renderPlotly(portfolio_index_data_tbl())
  
  # 3.4 Plot Header of portfolio ----
  portfolio_selection_triggered <- eventReactive(input$submit,{
    str_glue("Portfolio built from the following: {get_symbol_from_user_input(input$stock_1)}, 
             {get_symbol_from_user_input(input$stock_2)}, 
             {get_symbol_from_user_input(input$stock_3)}, 
             {get_symbol_from_user_input(input$stock_4)}")
  }, ignoreNULL = FALSE)
  
  output$plot_header_2 <- renderText(
    portfolio_selection_triggered()
  )
  
  # 3.5 Portfolio investment index with MAVG plot ----
  portfolio_index_mavg_data_tbl <- reactive({
    portfolio_data_mavg_tbl()  %>% 
      plot_portfolio_index_mavg()
  })
  output$portfolio_index_mavg_data_tbl <- renderPlotly(portfolio_index_mavg_data_tbl())
  
  # 3.6 Favourite Plots ----
  
  output$stock_charts <- renderUI({
    
    # # Last Analysis 
    # tab_panel_last_analysis <- tabPanel(
    #   title = "Last Analysis",
    #   div(
    #     class = "panel",
    #     div(
    #       class = "panel-header",
    #       h4(input$stock_1 %>% get_symbol_from_user_input())),
    #     div(
    #       class = "panel-body",
    #       input$stock_1 %>%
    #         get_symbol_from_user_input() %>% 
    #         single_asset_price(end = end(),
    #                            start = start()) %>% 
    #         stock_mavg_calculation(mavg_short = mavg_short(), # calculate moving avg 
    #                                mavg_long = mavg_long()) %>% 
    #         plot_stock_mavg()
    #       
    #     )
    #   )
    # )
    
    # First Tab Panel 
    tab_panel_individual_stocks <- tabPanel(
      title = "Stock Portfolio Analysis",
      
      panel_card(
        title = textOutput(outputId = "plot_header"),
        plotlyOutput(outputId = "portfolio_price_data_tbl")
      )
    )
    
    # Favourite Panels 
    favourite_tab_panels <- NULL #start as null so it will invalidate the '...' parameters
    if (length(reactive_values$favourites_list[[1]]) > 0) { #run the following code if the favourites list is >1
      favourite_tab_panels <- reactive_values$favourites_list %>% # is a vector of tickers
        map(.f = function(x) { # create an anonymous function 
          
          tabPanel(
            title = x, 
            
            panel_card(
              title = str_glue("Stock Price of {x}"), 
              x %>% # replace the initial value with the first vector
                single_asset_price(end = end(),
                                   start = start()) %>% 
                stock_mavg_calculation(mavg_short = mavg_short(), # calculate moving avg 
                                       mavg_long = mavg_long()) %>% 
                plot_stock_mavg()
            )
          )
        })
    }
    

    # Building the Tabset Panel
    do.call(
      what = tabsetPanel,
      args = list(tab_panel_individual_stocks) %>% 
        append(favourite_tab_panels) %>% 
        append(list(id = "tab_panel_stock_chart", type = "pills", selected = selected_tab()))
    )
  })
      
  output$portfolio_charts <- renderUI({
    div(
      class = "panel",
      div(
        class = "panel-header",
        h4(textOutput(outputId = "plot_header_2"))),
      div(
        class = "panel-body",
        plotlyOutput(outputId = "portfolio_index_mavg_data_tbl")
      )
    )
  })
  
  
  # 4.0 COMMENTARY ----
  # 4.1 Generate Commentary ----
  output$portfolio_commentary <- renderText(
    portfolio_data_mavg_tbl() %>%
      generate_portfolio_commentary()
  )
  
  # 5.0 RENDER WEBSITE ----
  
  output$website <- renderUI({
    
    req(credentials()$user_auth) #web page is displayed when user authentication is correct
    
    navbarPage(
    title = "Portfolio Analytics Prediction App",
    inverse = TRUE,
    collapsible = TRUE,
    theme = shinytheme("darkly"), # Sets up the website with darkly theme
    id = "inNavset",
    
    # In navbarpage, this header will show the username in every tab 
    header = div( 
      class = "pull-right",
      style = "padding-right: 20px;",
      p("Welcome, ", reactive_values$user_name)
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
      
      
    ),
    
    # 5.2.0 STOCK ANALYSIS ----
    tabPanel(
      value = "page_1",
      id = "section_1",
      title = "Stock Analysis",
      
      # CSS ----
      # shinythemes::themeSelector(), # selects the theme of the app
      
      # 5.2.1 HEADER ----
      div(
        class = "container",
        id = "page_1_header",
        h1(class = "page-header",
           "Portfolio Analytics Prediction App",
           tags$small("In Shiny")),
        p(class = "lead", "This is the beta version of my first portfolio analytics project in Shiny"),
        p(tags$strong("Stock List (Pick Between One to Five Stocks to Analyze Portfolio)"))
      ),
      
      # 5.2.2 FAVOURITES ----
      div(
        class = "container hidden-sm hidden-xs",
        id = "favourite_container",
        
        # 5.3.1 USER INPUTS ----
        div(
          class = "", #7.0 project setup
          column(
            width = 12,
            h5(class = "pull-left", "Favourites"), #pulls everything on the same column to the left
            actionButton(inputId = "favourites_clear", 
                         "Clear Favourites",
                         class = "pull-right"), # pulls this to the right
            actionButton(inputId = "favourites_toggle",
                         "Show/Hide", class = "pull-right")
          )
        ),
        
        # 5.3.2 FAVOURITE CARDS ----
        div(
          class = "row",
          id = "favourite_cards_section",
          uiOutput(outputId = "favourite_cards", class = "container")
        )
      ),
      
      
      
      # 5.4 APPLICATION UI ----
      div(
        class = "container",
        id = "application_ui",
        
        # 5.4.1 USER INPUTS ---- 
        div(
          class = "col-sm-4",
          fluidRow(
            column(
              id = "1",
              class = "well",
              width = 8,
              pickerInput(inputId = "stock_1",
                          label = h5("Stock 1"),
                          choices = stock_list_tbl$label,
                          multiple = FALSE,
                          selected = stock_list_tbl %>% filter(label %>% str_detect("AAPL")) %>% pull(label),
                          # filter(label %>% str_detect(pattern = paste0(reactive_values$last_symbol, ","))) #for GOOG and GOOGL issue
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
              width = 4,
              numericInput("w1", h5("    %"), 25,
                           min = 1, max = 100),
              
              numericInput("w2", h5("   %"), 25,
                           min = 1, max = 100),
              
              numericInput("w3", h5("   %"), 25,
                           min = 1, max = 100),
              
              numericInput("w4", h5("   %"), 25,
                           min = 1, max = 100)
            ),
          ),
          fluidRow(
            column(
              class = "well",
              width = 12,
              div(
                id = "input_buttons",
                actionButton(inputId = "submit",
                             label = "Submit",
                             icon = icon("piggy-bank",
                                         lib = "font-awesome")),
                actionButton(inputId = "reset",
                             label = "Reset",
                             icon = icon("sync", 
                                         lib = "font-awesome")),
                div(
                  class = "pull-right",
                  actionButton(inputId = "favourites_add",
                               label = NULL,
                               icon = icon("heart", lib = "font-awesome")),
                  actionButton(inputId = "settings_toggle",
                               label = NULL,
                               icon = icon("cog", 
                                           lib = "font-awesome"))
                  
                )
              )
            )
          ),
          fluidRow(
            column(
              class = "well",
              width = 12,
              div(
                id = "input_settings",
                dateInput("start_date",
                          h4("Starting Date"),
                          reactive_values$user_settings %>% pluck(1)%>% pull(start_date),
                          format = "yyyy-mm-dd"),
                dateInput("end_date",
                          h4("End Date"),
                          reactive_values$user_settings %>% pluck(1) %>% pull(end_date),
                          format = "yyyy-mm-dd"),
                hr(),
                sliderInput(inputId = "mavg_short", label = "Short Moving Average", 
                            min = 5, max = 40, value = reactive_values$user_settings %>% pluck(1) %>% pull(mavg_short)),
                sliderInput(inputId = "mavg_long", label = "Long Moving Average", 
                            min = 20, max = 120, value = reactive_values$user_settings %>% pluck(1) %>% pull(mavg_long)),
                actionButton(inputId = "apply_and_save", label = "Apply & Save", 
                             icon = icon("save"))
                
              )%>% shinyjs::hidden()
            )  
          )
        ),
        
        
        # 5.4.2 PLOT PANEL ----
        div(
          class = "col-sm-8",
          uiOutput(outputId = "stock_charts"),
          uiOutput(outputId = "portfolio_charts")
          
        )
        
        
      ),
      
      # 5.5.0 PORTFOLIO COMMENTARY ----
      div(
        class = "container",
        id = "commentary",
        column(
          width = 12,
          class = "panel",
          div(
            class = "panel-header",
            h4("Analyst Commentary")),
          div(
            class = "panel-body",
            textOutput(outputId = "portfolio_commentary") 
            #error if duration of months is less than mavg_long value
            
          )
        )
      )
      
      
    ),
    
    
    # 5.6.0 PORTFOLIO ANALYSIS ----
    tabPanel(
      title = "Portfolio Analysis",
      id = "section_2",
      value = "page_2",
      h1("Placeholder"),
      p("Placeholder")
    ),
    
    # 5.7.0 SENTIMENT ANALYSIS ----
    # tabPanel(
    #   title = "Sentiment Analysis",
    #   id = "section_4",
    #   value = "page_4",
    #   h1("Placeholder"),
    #   sidebarLayout(
    #     sidebarPanel(
    #       uiOutput("sentiment_search"),
    #       textOutput("sentiment"), 
    #       br(),
    #       sliderInput(
    #         inputId = "n_tweets",
    #         label   = "Number of tweets:",
    #         min     = 1,
    #         max     = 1500,
    #         value   = 200),
    #       shiny::textInput(inputId = "location", label = "Location", value = "Toronto, ON"),
    #       sliderInput(
    #         inputId = "n_miles",
    #         label   = "Twitter Search Radius (miles)",
    #         min     = 1,
    #         max     = 1500,
    #         value   = 1000),
    #       shiny::actionButton(inputId = "submit_2", "Submit", class = "btn-primary")
    #     ),
    #     
    #     # Show a plot of the generated distribution
    #     mainPanel(
    #       div(
    #         class = "row",
    #         div(
    #           class = "col-sm-8 panel",
    #           div(class = "panel-heading", h5("Sentiment Polarity")),
    #           div(class = "panel-body", plotlyOutput(outputId = "plotly", height = "250px"))
    #         ),
    #         div(
    #           class = "col-sm-4 panel",
    #           div(class = "panel-heading", h5("Tweet Proximity")),
    #           div(class = "panel-body", leafletOutput(outputId = "leaflet", height = 250))
    #         )
    #       ),
    #       
    #       div(
    #         class = "row",
    #         div(
    #           class = "col-sm-12 panel",
    #           div(class = "panel-heading", h5("Sentiment Word Cloud")),
    #           div(class = "panel-body", plotOutput(outputId = "wordcloud", height = "400px"))
    #         )
    #       )
    #   )
    #   )
    # ),
    
    # 5.8.0 OTHERS ----
    tabPanel(
      title = "About the Author",
      id = "section_3",
      value = "page_3",
      h1("Placeholder"),
      p("Placeholder")
    ),
    navbarMenu(
      title = "Resources",
      tabPanel(
        title = "BSU"
      ),
      tabPanel(
        title = "Reproducible Finance"
      ),
      "----",
      tabPanel(
        title = "Author's Linkedin",
        value = "page_10",
        h1("Linkin"),
        div(
          shiny::actionButton(
            inputId = "btn_1",
            label = "Author's Linkedin Profile - Click Me",
            class = "btn btn-primary btn-lg",
            icon = icon(name = "linkedin-in",
                        lib = "font-awesome"),
            onclick ="window.open('https://www.linkedin.com/in/stephenlung/', '_blank')"
            
          )
        )
      )
    )
    
    
    
    
    # 
    #   
  )
    
    
    
  })
  
  
  # 5.0 SENTIMENT ----
  # 
  # output$sentiment_search <- renderUI({
  #   input_stock <- input$stock_1 %>% get_symbol_from_user_input(num = 2) %>%   str_split(pattern = " ") %>% 
  #     pluck(1, 1)
  #   shiny::textInput(inputId = "query_test", label = "Topic / Hashtag", value = input_stock)
  # })
  # 
  # # verbatim output the input
  # output$sentiment <- renderText({
  #   # print(input$query_test)
  #   input$query_test
  # })
  # 
  # # 5.1 Setup Reactive Values ----
  # rv <- reactiveValues()
  # 
  # observeEvent(input$submit_2, {
  #   # Process data
  #   
  #   rv$geocode <- input$location %>% geocode_for_free() %>% near_geocode(input$n_miles)
  #   
  #   # CHANGE - Can update the input$stock_1 to be query_test 
  #   rv$data <-  search_tweets(
  #     q           = input$stock_1 %>% get_symbol_from_user_input(num = 2) %>% str_split(pattern = " ") %>% pluck(1, 1),
  #     # q = input$query_test,
  #     n           = input$n_tweets,
  #     include_rts = FALSE,
  #     geocode     = rv$geocode,
  #     lang        = "en",
  #     token       = token
  #   )
  # 
  #   rv$tweet_sentiment <- rv$data %>%
  #     select(text) %>%
  #     rowid_to_column() %>%
  #     unnest_tokens(word, text) %>%
  #     inner_join(get_sentiments("bing"))
  # 
  # }, ignoreNULL = FALSE)
  # 
  # # 5.2 Plotly ----
  # output$plotly <- renderPlotly({
  #   req(rv$tweet_sentiment, rv$data)
  # 
  #   sentiment_by_row_id_tbl <- rv$tweet_sentiment %>%
  #     select(-word) %>%
  #     count(rowid, sentiment) %>%
  #     pivot_wider(names_from = sentiment, values_from = n, values_fill = list(n = 0)) %>%
  #     mutate(sentiment = positive - negative) %>%
  #     left_join(
  #       rv$data %>% select(screen_name, text) %>% rowid_to_column()
  #     )
  # 
  #   label_wrap <- label_wrap_gen(width = 60)
  # 
  #   data_formatted <- sentiment_by_row_id_tbl %>%
  #     mutate(text_formatted = str_glue("Row ID: {rowid}
  #                                    Screen Name: {screen_name}
  #                                    Text:
  #                                    {label_wrap(text)}"))
  # 
  #   g <- data_formatted %>%
  #     ggplot(aes(rowid, sentiment)) +
  #     geom_line(color = "#2c3e50", alpha = 0.5) +
  #     geom_point(aes(text = text_formatted), color = "#2c3e50") +
  #     geom_smooth(method = "loess", span = 0.25, se = FALSE, color = "blue") +
  #     geom_hline(aes(yintercept = mean(sentiment)), color = "blue") +
  #     geom_hline(aes(yintercept = median(sentiment) + 1.96*IQR(sentiment)), color = "red") +
  #     geom_hline(aes(yintercept = median(sentiment) - 1.96*IQR(sentiment)), color = "red") +
  #     theme_tq() +
  #     labs(x = "Twitter User", y = "Sentiment")
  # 
  #   ggplotly(g, tooltip = "text") %>%
  #     layout(
  #       xaxis = list(
  #         rangeslider = list(type = "date")
  #       )
  #     )
  # 
  # })
  # 
  # # 5.3 Leaflet -----
  # output$leaflet <- renderLeaflet({
  #   
  #   req(rv$geocode)
  #   
  #   data_prepared <- tibble(
  #     location = rv$geocode
  #   ) %>%
  #     separate(location, into = c("lat", "lon", "distance"), sep = ",", remove = FALSE) %>%
  #     mutate(distance = distance %>% str_remove_all("[^0-9.-]")) %>%
  #     mutate_at(.vars = vars(-location), as.numeric) 
  #   
  #   data_prepared %>%
  #     leaflet() %>%
  #     setView(data_prepared$lon, data_prepared$lat, zoom = 3) %>%
  #     addTiles() %>%
  #     addMarkers(~lon, ~lat, popup = ~as.character(location), label = ~as.character(location)) %>%
  #     addCircles(lng = ~lon, lat = ~lat, weight = 1, radius = ~distance/0.000621371)
  #   
  # })
  # 
  # # 5.4 Wordcloud ----
  # output$wordcloud <- renderPlot({
  #   
  #   req(rv$data)
  #   
  #   tweets_tokenized_tbl <- rv$data %>%
  #     select(text) %>%
  #     rowid_to_column() %>%
  #     unnest_tokens(word, text)
  #   
  #   sentiment_bing_tbl <- tweets_tokenized_tbl %>%
  #     inner_join(get_sentiments("bing"))
  #   
  #   sentiment_by_word_tbl <- sentiment_bing_tbl %>%
  #     count(word, sentiment, sort = TRUE) 
  #   
  #   sentiment_by_word_tbl %>%
  #     # slice(1:100) %>%
  #     mutate(sentiment = factor(sentiment, levels = c("positive", "negative"))) %>%
  #     ggplot(aes(label = word, color = sentiment, size = n)) +
  #     geom_text_wordcloud_area() + 
  #     facet_wrap(~ sentiment, ncol = 2) +
  #     theme_tq(base_size = 30) +
  #     scale_color_tq() +
  #     scale_size_area(max_size = 16) 
  # })
  
  
}


# RUN APP ----
shinyApp(ui = ui, server = server)

