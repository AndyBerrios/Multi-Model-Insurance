library(shiny)
library(DT)
library(tidyverse) 
library(here)

source(here("EDA.R"))
source(here('comparison.R'))


server <- function(input, output){
  ################################################
  # data
  output$insurance_prem <- renderDT({
    datatable(insurance_data, options = list(pageLength = 10))
  })
  ################################################
  # hist of charges
  output$chrg_hist <- renderPlot({
    ggplot(insurance_data, aes(charges)) + 
      geom_histogram(binwidth = 500) +
      xlab('Charges in USD') + 
      ylab('Frequency')
  })
  
  # scatter of cont. features
  output$Distribution_Plot <- renderPlot({
    ggplot(insurance_data, aes(charges, .data[[input$features]], color = .data[[input$features]])) +
      geom_point(alpha = .5) +
      xlab('Charges in USD')
  })
  
  #geom ridges of factor features
  output$fact_dist_plot <- renderPlot({
    ggplot(insurance_data, aes(charges, .data[[input$fact_features]], fill = .data[[input$fact_features]])) +
      geom_density_ridges(alpha = .7)
  })
  ################################################
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
  
  ################################################
  output$pred_vs_actual <- renderPlot({
    final_res %>%
      collect_predictions() %>%
      ggplot(aes(x = .pred, y = charges)) +
      geom_point(alpha = 0.4) +
      geom_abline(color = 'red', linetype = "dashed") +
      labs(title = "Predicted vs. Actual Charges",
           x = "Predicted Charges", y = "Actual Charges")
  })
  
  output$residual_plot <- renderPlot({
    final_res %>%
      collect_predictions() %>%
      mutate(resid = charges - .pred) %>%
      ggplot(aes(x = .pred, y = resid)) +
      geom_point(alpha = 0.4) +
      geom_hline(yintercept = 0, linetype = "dashed") +
      labs(title = "Residuals vs. Predicted Charges", x = "Predicted Charges", y = "Residuals")
  })

  output$best_model_metrics <- renderDT({
    final_res %>%
      collect_metrics() %>%
      datatable(options = list(pageLength = 5, scrollX = TRUE))
  })
  
  
  output$var_importance_plot <- renderPlot({
    final_res$.workflow[[1]] %>%
      extract_fit_parsnip() %>%
      vip::vip(num_features = 10) + 
      ggtitle("Top 10 Important Variables")
  })
  
  ################################################
  
  
  # Reactive input
  user_input <- reactive({
    req(input$predict_btn)
    
    tibble(
      age = input$age,
      sex = input$sex,
      bmi = input$bmi,
      children = input$children,
      smoker = input$smoker,
      region = input$region
    )
  })
  
  
  # Prediction
  output$predicted_output <- renderText({
    req(user_input())
    
    # Prep input for model
    new_data <- user_input()
    wf <- final_res$.workflow[[1]]
    
    pred <- predict(wf, new_data = new_data)$.pred
    
    paste0("Estimated Charges: $", format(round(pred, 2), big.mark = ","))
  })
  
}

