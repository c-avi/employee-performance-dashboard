#Install and Load Required Packages
install.packages(c("shiny", "ggplot2", "dplyr", "DT", "readr"))
library(shiny)
library(ggplot2)
library(dplyr)
library(DT)
library(readr)

#Define the User Interface (UI)
?fluidPage
?titlePanel
?sidebarLayout
?fileInput
?uiOutput
?sliderInput
?verbatimTextOutput
?tabsetPanel
?DTOutput
ui <- fluidPage(
  titlePanel("Employee Performance Dashboard"),
  
  sidebarLayout(
    sidebarPanel(
      fileInput("file", "Upload CSV File", accept = ".csv"),
      uiOutput("departmentSelector"),
      uiOutput("jobRoleSelector"),
      sliderInput("incomeRange", "Monthly Income Range:",
                  min = 1000, max = 20000, value = c(1000, 20000), step = 500),
      sliderInput("ageRange", "Age Range:",
                  min = 18, max = 60, value = c(18, 60), step = 1)
    ),
    
    mainPanel(
      tabsetPanel(
        tabPanel("Summary",
                 verbatimTextOutput("summary")),
        tabPanel("Data",
                 DTOutput("table")),
        tabPanel("Plots",
                 plotOutput("agePlot"),
                 plotOutput("incomePlot"))
      )
    )
  )
)

#Define the Server Logic
?reactive
?renderUI
server <- function(input, output, session) {
  data <- reactive({
    req(input$file)
    df <- read_csv(input$file$datapath)
    
    # Update UI elements dynamically based on the uploaded data
    updateSelectInput(session, "departmentSelector", 
                      choices = unique(df$Department))
    updateSelectInput(session, "jobRoleSelector", 
                      choices = unique(df$JobRole))
    
    df
  })
  
  output$departmentSelector <- renderUI({
    selectInput("department", "Select Department:",
                choices = unique(data()$Department),
                selected = unique(data()$Department)[1])
  })
  
  output$jobRoleSelector <- renderUI({
    selectInput("jobRole", "Select Job Role:",
                choices = unique(data()$JobRole),
                selected = unique(data()$JobRole)[1])
  })
  
  output$summary <- renderPrint({
    summary(data())
  })
  
  output$table <- renderDT({
    df <- data() %>%
      filter(MonthlyIncome >= input$incomeRange[1],
             MonthlyIncome <= input$incomeRange[2],
             Age >= input$ageRange[1],
             Age <= input$ageRange[2],
             Department %in% input$department,
             JobRole %in% input$jobRole)
    datatable(df)
  })
  
  output$agePlot <- renderPlot({
    df <- data() %>%
      filter(MonthlyIncome >= input$incomeRange[1],
             MonthlyIncome <= input$incomeRange[2],
             Age >= input$ageRange[1],
             Age <= input$ageRange[2],
             Department %in% input$department,
             JobRole %in% input$jobRole)
    
    ggplot(df, aes(x = Age)) +
      geom_histogram(binwidth = 5, fill = "blue", color = "black") +
      labs(title = "Age Distribution", x = "Age", y = "Frequency")
  })
  
  output$incomePlot <- renderPlot({
    df <- data() %>%
      filter(MonthlyIncome >= input$incomeRange[1],
             MonthlyIncome <= input$incomeRange[2],
             Age >= input$ageRange[1],
             Age <= input$ageRange[2],
             Department %in% input$department,
             JobRole %in% input$jobRole)
    
    ggplot(df, aes(x = MonthlyIncome)) +
      geom_histogram(binwidth = 1000, fill = "green", color = "black") +
      labs(title = "Monthly Income Distribution", x = "Monthly Income", y = "Frequency")
  })
}

#Run the Application
?shinyApp
shinyApp(ui = ui, server = server)
