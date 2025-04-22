library(shiny)
library(DT)
library(tidyverse) 
library(here)

source(here("EDA.R"))
source(here('comparison.R'))


server <- function(input, output){
  # data
  output$insurance_prem <- renderDT({
    datatable(insurance_data, options = list(pageLength = 10))
  })
  
  # hist of charges
  output$chrg_hist <- renderPlot({
    ggplot(insurance_data, aes(charges)) + 
      geom_histogram(binwidth = 500) +
      xlab('Charges in USD') + 
      ylab('Frequency')
  })
  
  # scatter of cont. features
  output$Distribution_Plot <- renderPlot({
    ggplot(insurance_data, aes(charges, .data[[input$features]])) +
      geom_point(alpha = .5) +
      xlab('Charges in USD')
  })
  
  #geom ridges of factor features
  output$fact_dist_plot <- renderPlot({
    ggplot(insurance_data, aes(charges, .data[[input$fact_features]], fill = .data[[input$fact_features]])) +
      geom_density_ridges(alpha = .7)
  })
  
  # table of result metrics
  output$model_metrics_table <- renderDT({
    datatable(all_metrics, options = list(pageLength = 8))
  })
  
  # bar of estimate from each model
  output$metrics_barplot <- renderPlot({
    ggplot(all_metrics, aes(x = model, y = .estimate, fill = model)) +
      geom_col() +
      facet_wrap(~ .metric, scales = "free_y") +
      labs(title = "Model Performance", y = "Metric Value", x = "Model") +
      theme_minimal()
  })
  
  
  
}

# shinyApp(ui, server)
