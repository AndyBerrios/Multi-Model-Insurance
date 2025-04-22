
library(shiny)
library(shinydashboard)
library(DT)
# library(semeantics.dashboard)


ui <- dashboardPage(skin = 'green',
  dashboardHeader( title = 'Insurance ML Model Dashboard'),
  dashboardSidebar(
    sidebarMenu(  # wrap with sidebarMenu
      menuItem("Data", tabName = "data", icon = icon("table")),
      menuItem("Data Exploration", tabName = "data_EDA", icon = icon("chart-simple")),
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
              box(
                tags$h2("Distribution of Charges"),
                plotOutput("chrg_hist"),
                width = 12
              ),
              
              fluidRow(
                box(
                  tags$h2("Continuous Features distribution of charges"),
                  plotOutput('Distribution_Plot'),
                  width = 8
                ),
                box(
                  selectInput('features', 'Features:', c("age", "bmi")),
                  width = 4
                )
              ),
              
              # Box 3: Factor Features Distribution
              fluidRow(
                box(
                  tags$h2("Factor Features distribution of charges"),
                  plotOutput('fact_dist_plot'), width = 8
                ),
                box(
                  selectInput('fact_features', 'Features:', c("sex", "children", "smoker", "region")), width = 4
                ))
              ),
      
      tabItem(tabName = 'compare',
              tabItem(tabName = "compare",
                      box(tags$h2("Model Performance Metrics"), width = 12),
                      fluidRow(
                        box(DTOutput("model_metrics_table"), width = 6),
                        box(plotOutput("metrics_barplot"), width = 6)
                      )
              )),
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


