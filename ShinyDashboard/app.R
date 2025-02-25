library(shiny)
library(shinydashboard)
library(ggplot2)
library(dplyr)
library(lubridate)
library(readr)
library(plotly)

# Load the dataset
data <- read_csv("../businessdata.csv")

# Convert date column
data$Purchase_Date <- as.Date(data$Purchase_Date, format="%Y-%m-%d")
data$Month <- format(data$Purchase_Date, "%Y-%m")  # Extract Year-Month for grouping

# UI
ui <- dashboardPage(
  dashboardHeader(title = "Sales Dashboard"),
  dashboardSidebar(
    selectInput("month", "Select Month:", choices = c("All", unique(data$Month)), selected = "All"),
    selectInput("region", "Select Region:", choices = c("All", unique(data$Customer_Region)), selected = "All"),
    selectInput("category", "Select Category:", choices = c("All", unique(data$Category)), selected = "All")
  ),
  
  dashboardBody(
    fluidRow(
      valueBoxOutput("total_transactions", width = 4),
      valueBoxOutput("total_sales", width = 4),
      valueBoxOutput("top_category", width = 4)
    ),
    fluidRow(
      box(plotlyOutput("sales_trend_plot", height = "450px"), width = 12)
    ),
  )
)

# Server
server <- function(input, output) {
  
  filtered_data <- reactive({
    df <- data
    if (input$month != "All") df <- df %>% filter(Month == input$month)
    if (input$region != "All") df <- df %>% filter(Customer_Region == input$region)
    if (input$category != "All") df <- df %>% filter(Category == input$category)
    df
  })
  
  # Total Sales (SUM of Total_Cost)
  output$total_sales <- renderValueBox({
    total_sales <- sum(filtered_data()$Total_Cost, na.rm = TRUE)  # Ensure sum is correct
    valueBox(
      paste0("$", format(total_sales, big.mark = ",")), "Total Sales", 
      icon = icon("dollar-sign"),
      color = "green"
    )
  })
  
  # Total Transactions (COUNT of rows)
  output$total_transactions <- renderValueBox({
    total_trans <- nrow(filtered_data())  # Count the number of transactions
    valueBox(
      format(total_trans, big.mark = ","), "Total Transactions", 
      icon = icon("shopping-cart"),
      color = "orange"
    )
  })
  
  # Top-Selling Category
  output$top_category <- renderValueBox({
    top_cat <- filtered_data() %>% 
      group_by(Category) %>% 
      summarize(Total_Sales = sum(Total_Cost, na.rm = TRUE)) %>% 
      arrange(desc(Total_Sales)) %>% 
      slice_head(n = 1) %>% 
      pull(Category)
    
    valueBox(
      ifelse(length(top_cat) > 0, top_cat, "No Data"), "Top-Selling Category", icon = icon("chart-bar"),
      color = "blue"
    )
  })
  
  
  # Sales Trend Plot
  output$sales_trend_plot <- renderPlotly({
    df <- filtered_data() %>% group_by(Purchase_Date) %>% summarize(Total_Sales = sum(Total_Cost, na.rm = TRUE))
    
    p <- ggplot(df, aes(x = Purchase_Date, y = Total_Sales)) +
      geom_line(color = "blue") +
      geom_point(color = "red") +
      labs(title = "Sales Trend", x = "Date", y = "Total Sales") +
      theme_minimal() +
      theme(text = element_text(size = 12))  # Improve font readability
    
    ggplotly(p)
  })
  
}

# Run the app
shinyApp(ui, server)
