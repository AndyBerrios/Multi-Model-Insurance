
library(shiny)
library(shinydashboard)
library(DT)

ui <- dashboardPage(
  dashboardHeader( title = 'Insurance ML Model Dashboard'),
  dashboardSidebar(
    sidebarMenu(  # wrap with sidebarMenu
      menuItem("Data", tabName = "data", icon = icon("table")),
      menuItem("Data Exploration", tabName = "data_EDA", icon = icon("table")),
      menuItem("Model Comparison", tabName = "compare", icon = icon("chart-bar")),
      menuItem("Model Output", tabName = "output", icon = icon("robot")),
      menuItem("Live Prediction", tabName = "predict", icon = icon("sliders-h"))
    )
  ),
  dashboardBody(
    tabItems(
      tabItem(tabName = "data",
              fluidPage( h1('Insurance Premium Data'),
                dataTableOutput('insurance_prem'))
              ),
      tabItem(tabName = "data_EDA",
              box(plotOutput('Distribution_Plot'), width = 8),
              box(
                selectInput('features', 'Features:',
                            c("age",
                              "sex",
                              "bmi",
                              "children", 
                              "smoker",
                              'region')), width = 4)
              ),
      tabItem(tabName = 'compare',
              fluidPage(
                h1('testing'))
              ),
      tabItem(tabName = 'output',
              fluidPage(
                h1('testing 2 '))
              ),
      tabItem(tabName = 'predict',
              fluidPage(
                h1('testing 3'))
              )
      )
    )
  )



