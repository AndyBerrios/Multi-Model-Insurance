
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
      tabItem(tabName = "output",
              fluidRow(
                box(title = "Predicted vs Actual", plotOutput("pred_vs_actual"), width = 6),
                box(title = "Residuals Plot", plotOutput("residual_plot"), width = 6)
              ),
              fluidRow(
                box(title = "Model Metrics", DTOutput("best_model_metrics"), width = 6),
                box(title = "Variable Importance", plotOutput("var_importance_plot"), width = 6) # if applicable
              )
              ),
      tabItem(tabName = "predict",
              fluidRow(
                box(title = "Enter Patient Info", width = 4,
                    numericInput("age", "Age:", value = 30, min = 18, max = 100),
                    selectInput("sex", "Sex:", choices = c("male", "female")),
                    numericInput("bmi", "BMI:", value = 28),
                    selectInput("children", "Children:", choices = as.character(0:5)) ,
                    selectInput("smoker", "Smoker:", choices = c("yes", "no")),
                    selectInput("region", "Region:", choices = c("northeast", "northwest", "southeast", "southwest")),
                    actionButton("predict_btn", "Predict")
                ),
                box(title = "Predicted Charges", width = 8,
                    h3(textOutput("predicted_output"))
                )
              )
      )
      
      )
    )
  )


