library(shiny)
library(ggplot2)
library(dplyr)
library(DT)
library(plotly)

server <- function(input, output, session) {
  
  output$summary <- renderPrint({
    "Placeholder for summary statistics"
  })
  
  output$hist_plot <- renderPlot({
    plot(1, 1, main = "Histogram Placeholder")
  })
  
  output$metrics_table <- renderDT({
    datatable(data.frame(Model = c("LM", "GLM", "RF", "XGB"), RMSE = runif(4, 3000, 6000)))
  })
  
  output$compare_plot <- renderPlotly({
    df <- data.frame(Model = c("LM", "GLM", "RF", "XGB"), RMSE = runif(4, 3000, 6000))
    gg <- ggplot(df, aes(x = Model, y = RMSE, fill = Model)) + geom_bar(stat = "identity")
    ggplotly(gg)
  })
  
  output$pred_vs_actual <- renderPlot({
    plot(1, 1, main = paste("Predicted vs Actual for", input$model_choice))
  })
  
  observeEvent(input$goButton, {
    output$live_prediction <- renderPrint({
      paste("Prediction result for Age:", input$age, ", BMI:", input$bmi, ", Smoker:", input$smoker)
    })
  })
}

