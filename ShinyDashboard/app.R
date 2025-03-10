library(shiny)
library(tidyverse)
library(DT)
library(plotly)
library(tidyquant)  # For stock data

ui <- fluidPage(
  titlePanel("Stock Market Analytics"),
  
  sidebarLayout(
    sidebarPanel(
      selectizeInput("stock_symbol_1", "Select First Stock Symbol:", 
                     choices = c("AAPL", "DODGX", "FDTOX", "FLPSX", "GSAT", "ITOT", "KSPI", "OMX", "SAIL", "VRM", "VWO", "VXRT", "VXUS"),  
                     selected = c(),
                     multiple = TRUE,  
                     options = list(placeholder = "Search or Select Stock Symbols")),
      
      selectizeInput("stock_symbol_2", "Select Second Stock Symbol:", 
                     choices = c("AAPL", "DODGX", "FDTOX", "FLPSX", "GSAT", "ITOT", "KSPI", "OMX", "SAIL", "VRM", "VWO", "VXRT", "VXUS"),  
                     selected = c(), 
                     multiple = TRUE,  
                     options = list(placeholder = "Search or Select Stock Symbol")),
      
      dateRangeInput("date_range", "Select Date Range:",
                     start = Sys.Date() - 30, end = Sys.Date()),
      actionButton("fetch_stock", "Get Stock Data")
    ),
    
    mainPanel(
      tabsetPanel(
        tabPanel("Stock Data", DTOutput("stock_table")),
        tabPanel("Stock Price Trend", plotlyOutput("stock_plot"))
      )
    )
  )
)

server <- function(input, output, session) {
  
  stock_data <- reactiveVal(data.frame())
  
  observeEvent(input$fetch_stock, {
    req(length(input$stock_symbol_1) > 0, length(input$stock_symbol_2) > 0)  # Ensure at least one symbol is selected for both inputs
    
    # Fetch stock data for both symbols
    data_list <- list()  # To store the data for both symbols
    
    # Fetch stock data for all selected symbols in stock_symbol_1
    for (symbol in input$stock_symbol_1) {
      data <- tq_get(symbol, from = input$date_range[1], to = input$date_range[2])
      data$symbol <- symbol
      data_list[[symbol]] <- data
    }
    
    # Fetch stock data for all selected symbols in stock_symbol_2
    for (symbol in input$stock_symbol_2) {
      data <- tq_get(symbol, from = input$date_range[1], to = input$date_range[2])
      data$symbol <- symbol
      data_list[[symbol]] <- data
    }
    
    # Combine all data into one data frame
    combined_data <- bind_rows(data_list)
    
    stock_data(combined_data)
  })
  
  output$stock_table <- renderDT({
    req(stock_data())
    datatable(stock_data())
  })
  
  output$stock_plot <- renderPlotly({
    req(stock_data())
    
    plot <- ggplot(stock_data(), aes(x = date, y = adjusted, color = symbol)) +
      geom_line() +
      labs(title = paste("Stock Price Trend"), x = "Date", y = "Adjusted Close Price") +
      theme_minimal()
    
    ggplotly(plot)
  })
}

shinyApp(ui, server)
