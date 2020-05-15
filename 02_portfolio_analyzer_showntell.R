# LIBRARIES ----

# setwd("C:/Users/steph/Dropbox/Business University Science/Personal Portfolio/Part-5-Draft")
library(flexdashboard)
library(pacman)

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
source("00_scripts/info_card.R")


# UI ----
ui <- navbarPage(
  title = "Portfolio Analytics Prediction App",
  inverse = TRUE,
  collapsible = TRUE,
  theme = shinytheme("darkly"),
  id = "inNavset",
  
  
  
  tabPanel(
    title = "Home",
    value = "Home",
    
    #CSS to change contextual themes
    tags$head(
      tags$link(rel = "stylesheet",
                type = "text/css",
                href = "styles.css")
    ),
    
    # JS ----
    shinyjs::useShinyjs(),
    
    
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
      
      # 1.0 THUMBNAILS ----
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
  
  # 2.0 STOCK ANALYSIS ----
  tabPanel(
    value = "page_1",
    id = "section_1",
    title = "Stock Analysis",
    shinythemes::themeSelector(),
    # 2.1 HEADER ----
    div(
      class = "container",
      id = "page_1_header",
      h1(class = "page-header",
         "Portfolio Analytics Prediction App",
         tags$small("In Shiny")),
      p(class = "lead", "This is the beta version of my first portfolio analytics project in Shiny"),
      p(tags$strong("Stock List (Pick Between One to Five Stocks to Analyze Portfolio)"))
    ),
    
    # 2.2 FAVOURITES ----
    div(
      class = "container hidden-sm hidden-xs",
      id = "favourite_container",
      
      div(
        class = "container",
        column(
          width = 12,
          h5("Favourites")
        )
      ),
      div(
        class = "container",
        id = "favourite_cards",
        column(
          width = 3,
          info_card(
            title = "AAPL",
            value = p("20-Day", tags$small("vs 50-Day")),
            sub_value = "20%"
          )
        ),
        column(
          width = 3,
          info_card(
            title = "MSFT",
            value = p("20-Day", tags$small("vs 50-Day")),
            sub_value = "-23%",
            sub_text_color = "danger"
          )
        ),
        column(
          width = 3,
          info_card(
            title = "NFLX",
            value = p("20-Day", tags$small("vs 50-Day")),
            sub_value = "15%"
          )
        ),
        column(
          width = 3,
          info_card(
            title = "AMZN",
            value = p("20-Day", tags$small("vs 50-Day")),
            sub_value = "12%"
          )
        )
      )
    ),
    
    
    
    # 2.3 THREE WELL PANEL APPLICATION UI ----
    div(
      class = "container",
      id = "application_ui",
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
                        "2006-02-01",
                        format = "yyyy-mm-dd"),
              dateInput("end_date",
                        h4("End Date"),
                        today(),
                        format = "yyyy-mm-dd"),
              hr(),
              sliderInput(inputId = "mavg_short", label = "Short Moving Average", 
                          min = 5, max = 40, value = 20),
              
              sliderInput(inputId = "mavg_long", label = "Long Moving Average", 
                          min = 50, max = 120, value = 50) 
              
            )%>% shinyjs::hidden()
          )  
        )
      ),
      div(
        class = "col-sm-8",
        div(
          class = "panel",
          div(
            class = "panel-header",
            h4(textOutput(outputId = "plot_header"))),
          div(
            class = "panel-body",
            plotlyOutput(outputId = "portfolio_price_data_tbl")
            
          )
        ),
        div(
          class = "panel",
          div(
            class = "panel-header",
            h4(textOutput(outputId = "plot_header_2"))),
          div(
            plotlyOutput(outputId = "portfolio_index_mavg_data_tbl")
            
          )
        )
      )
      
      
    ),
    
    # 2.4 PORTFOLIO COMMENTARY ----
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
          # stock_data_tbl %>% 
          #   generate_commentary(user_input = user_input)
        )
      )
    )
    
    
  ),
  
  
  # 3.0 PORTFOLIO ANALYSIS ----
  tabPanel(
    title = "Portfolio Analysis",
    id = "section_2",
    value = "page_2",
    h1("Placeholder"),
    p("Placeholder")
  ),
  # 4.0 PORTFOLIO ANALYSIS ----
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

# SERVER ----
server <- function(input, output, session){
  
  # Toggle to unhide/hide settings ----
  observeEvent(input$settings_toggle, {
    shinyjs::toggle(id = "input_settings", anim = TRUE)
  })
  
  # Button selection to open tabpanel ----
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
  
  # Plot Header of stock ticker
  stock_selection_triggered <- eventReactive(input$submit,{
    str_glue("Stock prices of the following: {get_symbol_from_user_input(input$stock_1)}, 
             {get_symbol_from_user_input(input$stock_2)}, 
             {get_symbol_from_user_input(input$stock_3)}, 
             {get_symbol_from_user_input(input$stock_4)}")
  }, ignoreNULL = FALSE)
  
  output$plot_header <- renderText(
    stock_selection_triggered()
  )
  
  
  # Prices of each ticker Plot ----
  portfolio_price_data_tbl <- reactive({
    multi_asset_price_portfolio(symbols = symbols(), 
                                end = end(), 
                                start = start(), 
                                wts_tbl = wts_tbl()) %>% 
      plot_portfolio_price()
  })
  output$portfolio_price_data_tbl <- renderPlotly(portfolio_price_data_tbl())
  
  # Plot Header of portfolio
  portfolio_selection_triggered <- eventReactive(input$submit,{
    str_glue("Portfolio built from the following: {get_symbol_from_user_input(input$stock_1)}, 
             {get_symbol_from_user_input(input$stock_2)}, 
             {get_symbol_from_user_input(input$stock_3)}, 
             {get_symbol_from_user_input(input$stock_4)}")
  }, ignoreNULL = FALSE)
  
  output$plot_header_2 <- renderText(
    portfolio_selection_triggered()
  )
  
  
  # Portfolio data tbl
  portfolio_data_mavg_tbl <- reactive({
    multi_asset_price_portfolio(symbols = symbols(),
                                end = end(),
                                start = start(),
                                wts_tbl = wts_tbl()) %>%
      multi_asset_return_portfolio(period = "monthly") %>%
      wealth_index(wts_tbl = wts_tbl(), name_portfolio = "test portfolio") %>%
      mavg_calculation(mavg_short = input$mavg_short, 
                       mavg_long = input$mavg_long)
    
  })
  
  # Portfolio investment index Plot ----
  portfolio_index_data_tbl <- reactive({
    portfolio_data_mavg_tbl()  %>%
      plot_portfolio_index()
  })
  output$portfolio_index_data_tbl <- renderPlotly(portfolio_index_data_tbl())
  
  # Portfolio investment index with MAVG plot ----
  portfolio_index_mavg_data_tbl <- reactive({
    portfolio_data_mavg_tbl()  %>% 
      plot_portfolio_index_mavg()
    
    
  })
  output$portfolio_index_mavg_data_tbl <- renderPlotly(portfolio_index_mavg_data_tbl())
  
  # Generate Commentary ----
  output$portfolio_commentary <- renderText(
    portfolio_data_mavg_tbl() %>% 
      generate_portfolio_commentary()
  )
  
}


# RUN APP ----
shinyApp(ui = ui, server = server)

