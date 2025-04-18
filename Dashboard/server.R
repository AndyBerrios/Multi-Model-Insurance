library(shiny)
library(DT)
library(tidyverse)

source("EDA.R")

server <- function(input, output){
  output$Distribution_Plot <- renderPlot({
    ggplot(insurance_data, aes(charges, .data[[input$features]])) +
      geom_point(alpha = .5) +
      labs(title = 'Distribution of Charges')
  })
}

shinyApp(ui, server)
