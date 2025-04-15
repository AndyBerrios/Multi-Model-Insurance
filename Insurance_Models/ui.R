
library(shiny)
library(shinydashboard)

ui <- dashboardPage(
  dashboardHeader(title = "Insurance ML Model Dashboard"),
  dashboardSidebar(
    sidebarMenu(
      menuItem("Data Exploration", tabName = "data", icon = icon("table")),
      menuItem("Model Comparison", tabName = "compare", icon = icon("chart-bar")),
      menuItem("Model Output", tabName = "output", icon = icon("robot")),
      menuItem("Live Prediction", tabName = "predict", icon = icon("sliders-h"))
    )
  ),
  dashboardBody(
    tabItems(
      tabItem(tabName = "data",
              fluidRow(
                box(title = "Summary Stats", width = 6, verbatimTextOutput("summary")),
                box(title = "Distribution Plot", width = 6, plotOutput("hist_plot"))
              )
      ),
      
      tabItem(tabName = "compare",
              fluidRow(
                box(title = "Model Performance Table", width = 12, DTOutput("metrics_table"))
              ),
              fluidRow(
                box(title = "Metric Comparison", width = 12, plotlyOutput("compare_plot"))
              )
      ),
      
      tabItem(tabName = "output",
              fluidRow(
                box(title = "Choose Model", width = 4,
                    selectInput("model_choice", "Select Model:", 
                                choices = c("Linear Regression", "GLM", "Random Forest", "XGBoost"))
                ),
                box(title = "Predictions vs Actual", width = 8, plotOutput("pred_vs_actual"))
              )
      ),
      
      tabItem(tabName = "predict",
              fluidRow(
                box(title = "Input Features", width = 4,
                    numericInput("age", "Age:", value = 30, min = 18, max = 100),
                    numericInput("bmi", "BMI:", value = 25, min = 10, max = 50),
                    selectInput("smoker", "Smoker:", choices = c("yes", "no")),
                    actionButton("goButton", "Predict")
                ),
                box(title = "Prediction Output", width = 8, verbatimTextOutput("live_prediction"))
              )
      )
    )
  )
)