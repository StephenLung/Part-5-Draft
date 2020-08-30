library(shiny)
setwd("C:/Users/steph/Dropbox/Business University Science/Personal Portfolio/Part-5-Draft")

source("00_scripts/sentiment_analysis_functions.R")

ui <- fluidPage(
  
  # 2.0 STOCK ANALYSIS ----
  tabPanel(
    value = "page_1",
    id = "section_1",
    title = "Stock Analysis",
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
                        ))
          )
          
        )
      ),
      div(
        class = "col-sm-8",
        
        verbatimTextOutput(outputId = "stock"),
        # plotOutput(outputId = "plot")
        
        
      )
    )
  )
  
)

server <- function(input, output, session) {
  
  # Output the stock company name
  output$stock <- renderPrint(input$stock_1 %>% 
                                pull_company())
  
  # Output the sentiment table
  output$code <- renderPrint(input$stock_1 %>% 
                               setup_rv_data() %>% 
                               setup_rv_sentiment())
  # Output the sentiment plot
  output$plot <- renderPlot(input$stock_1 %>% 
                              setup_rv_data() %>% 
                              setup_rv_sentiment() %>% 
                              plot_sentiment(plotly = TRUE))
  
}

shinyApp(ui, server)