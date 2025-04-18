
library(shiny)
library(shinydashboard)

ui <- dashboardPage(
  dashboardHeader( title = 'Insurance ML Model Dashboard'),
  dashboardSidebar(menuItem("Data Exploration", tabName = "data", icon = icon("table")),
                   menuItem("Model Comparison", tabName = "compare", icon = icon("chart-bar")),
                   menuItem("Model Output", tabName = "output", icon = icon("robot")),
                   menuItem("Live Prediction", tabName = "predict", icon = icon("sliders-h"))),
  dashboardBody(
    box(plotOutput('Distribution_Plot'), width = 8),
    box(
      selectInput('features', 'Features:',
                  c("age",
                    "sex",
                    "bmi",
                    "children", 
                    "smoker",
                    'region')), width = 4
    ), 
    tabItems(
      tabItem(tabName = "data",
              fluidRow(
                box(title = "Summary Stats", width = 6, verbatimTextOutput("summary")),
                box(title = "Distribution Plot", width = 6, plotOutput("hist_plot"))
              )
      )
    )
  )
)
  



