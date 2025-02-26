library(shiny)
library(tidyr)
library(shinydashboard)
library(ggplot2)
library(dplyr)
library(lubridate)
library(readr)
library(plotly)

data <- read_csv("businessdata.csv")

# Convert date column
data$Purchase_Date <- as.Date(data$Purchase_Date, format="%Y-%m-%d")
data$Month <- format(data$Purchase_Date, "%Y-%m")  

# Identify churn (One-time vs. Repeat Customers)
customer_purchases <- data %>% group_by(Customer_ID) %>% summarize(Purchase_Count = n())
data <- data %>% left_join(customer_purchases, by = "Customer_ID")
data$Churn <- ifelse(data$Purchase_Count == 1, "One-time", "Repeat")

# UI
ui <- dashboardPage(
  dashboardHeader(title = tagList(
      tags$img(src = "logo.jpg", height = "40px", style = ""),
      "Sales Dashboard"
    )
  ),
  dashboardSidebar(
    selectizeInput("month", "Select Month:", 
                   choices = c("All", sort(unique(data$Month), decreasing = TRUE)),  
                   selected = "All",
                   options = list(placeholder = "Search or Select a Month")  
    ),
    selectizeInput("region", "Select Region:", 
                   choices = c("All", sort(unique(data$Customer_Region))),  
                   selected = "All",
                   options = list(placeholder = "Search or Select a Region")  
    ),
    selectizeInput("category", "Select Category:", 
                   choices = c("All", sort(unique(data$Category))), 
                   selected = "All",
                   options = list(placeholder = "Search or Select a Category")
    )
  ),
  
  dashboardBody(
    fluidRow(
      valueBoxOutput("total_transactions", width = 3),
      valueBoxOutput("total_sales", width = 3),
      valueBoxOutput("top_category", width = 3),
      valueBoxOutput("churn_rate", width = 3)  # New Churn KPI
    ),
    
    fluidRow(
      box(
        title = "Sales Trend",  
        status = "primary",  
        solidHeader = TRUE,  
        width = 6,  
        plotlyOutput("sales_trend_plot", height = "450px")
      ),
      box(
        title = "Total Sales by Region", 
        status = "primary",  
        solidHeader = TRUE,  
        width = 6, 
        plotlyOutput("sales_by_region_plot", height = "450px")
      )
    ),  
    fluidRow(
      box(
        title = "Churn Analysis (One-time vs. Repeat Customers)",
        status = "primary",
        solidHeader = TRUE,
        width = 6,
        plotlyOutput("churn_plot", height = "400px"),
        height = "600px"
      ),
      box(
        title = "Churn Summary by Region & Category",
        status = "primary",
        solidHeader = TRUE,
        width = 6,
        dataTableOutput("churn_table")
      )
    )
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
  
  custom_colors <- c("Central" = "#E74C3C",  
                     "East" = "#F1C40F",     
                     "North" = "#2ECC71",    
                     "South" = "#3498DB",    
                     "West" = "#9B59B6")     
  
  # Total Sales (SUM of Total_Cost)
  output$total_sales <- renderValueBox({
    total_sales <- sum(filtered_data()$Total_Cost, na.rm = TRUE)  
    valueBox(
      paste0("$", format(total_sales, big.mark = ",")), "Total Sales", 
      icon = icon("dollar-sign"),
      color = "green"
    )
  })
  
  # Total Transactions 
  output$total_transactions <- renderValueBox({
    total_trans <- nrow(filtered_data())  
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
    
    box_color <- case_when(
      top_cat == "Electronics" ~ "blue",
      top_cat == "Accessories" ~ "fuchsia",
      TRUE ~ "red"
    )
    
    valueBox(
      ifelse(length(top_cat) > 0, top_cat, "No Data"), 
      "Top-Selling Category", 
      icon = icon("chart-bar"),
      color = box_color
    )
  })
  
  # Churn Rate KPI
  output$churn_rate <- renderValueBox({
    churn_data <- filtered_data()
    churn_rate <- mean(churn_data$Churn == "One-time") * 100
    valueBox(
      paste0(round(churn_rate, 2), "%"), "Churn Rate (One-time Customers)",
      icon = icon("user-times"),
      color = "red"
    )
  })
  
  # Sales Trend Line Graph 
  output$sales_trend_plot <- renderPlotly({
    df <- filtered_data() %>%
      group_by(Purchase_Date, Customer_Region) %>%
      summarize(Total_Sales = sum(Total_Cost, na.rm = TRUE), .groups = 'drop')
    
    df$text <- paste0(
      "Date: ", df$Purchase_Date, "<br>",
      "Total Sales: $", format(df$Total_Sales, big.mark = ","), "<br>",
      "Region: ", df$Customer_Region
    )
    
    p <- ggplot(df, aes(x = Purchase_Date, y = Total_Sales, 
                        color = Customer_Region, group = Customer_Region, 
                        text = text)) +  
      geom_line(size = 0.5) +
      geom_point(size = 1) +
      scale_color_manual(values = custom_colors) +
      labs(x = "Date", y = "Total Sales") +
      theme_minimal() +
      theme(
        text = element_text(size = 12),
        plot.title = element_text(hjust = 0.5),
        legend.title = element_blank()
      )
    
    ggplotly(p, tooltip = "text")
  })
  
  # Sales by Region Bar Graph 
  output$sales_by_region_plot <- renderPlotly({
    df <- filtered_data() %>% 
      group_by(Customer_Region) %>% 
      summarize(Total_Sales = sum(Total_Cost, na.rm = TRUE), .groups = 'drop')
    
    df$text <- paste0(
      "Region: ", df$Customer_Region, "<br>",
      "Total Sales: $", format(df$Total_Sales, big.mark = ",")
    )
    
    p <- ggplot(df, aes(x = Customer_Region, y = Total_Sales, fill = Customer_Region, text = text)) +
      geom_bar(stat = "identity") +
      scale_fill_manual(values = custom_colors, na.translate = FALSE) +
      scale_y_continuous(labels = scales::comma) +  
      labs(x = "Customer Region", y = "Total Sales") +
      theme_minimal() +
      theme(
        legend.position = "none",
        text = element_text(size = 14),
        axis.title.y = element_text(angle = 0, vjust = 0.5),
        plot.title = element_text(hjust = 0.5)
      )
    
    ggplotly(p, tooltip = "text") %>% 
      layout(margin = list(l = 60))
  })
  
  # Churn Bar Plot
  output$churn_plot <- renderPlotly({
    df <- filtered_data() %>% group_by(Churn) %>% summarise(Total_Sales = sum(Total_Cost, na.rm = TRUE))
    
    p <- ggplot(df, aes(x = Churn, y = Total_Sales, fill = Churn)) +
      geom_bar(stat = "identity") +
      labs(x = "Customer Type", y = "Total Sales", title = "Sales by Customer Type") +
      theme_minimal()
    
    ggplotly(p)
  })
  
  # Churn Summary Table
  output$churn_table <- renderDataTable({
    churn_summary <- filtered_data() %>% 
      group_by(Customer_Region, Category, Churn) %>% 
      summarise(Total_Customers = n(), .groups = 'drop') %>% 
      spread(Churn, Total_Customers, fill = 0)
    churn_summary
  })
  
}

shinyApp(ui, server)