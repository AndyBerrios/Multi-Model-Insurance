library(shiny)
library(DT)
library(tidyverse)
library(here)


source(here("EDA.R"))


server <- function(input, output){
  output$insurance_prem <- renderDT({
    datatable(insurance_data, options = list(pageLength = 10))
  })
  
  
  output$Distribution_Plot <- renderPlot({
    ggplot(insurance_data, aes(charges, .data[[input$features]])) +
      geom_point(alpha = .5) +
      labs(title = 'Distribution of Charges') +
      xlab('Charges in USD')
  })
}

# shinyApp(ui, server)
