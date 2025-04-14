############################################
# data prep 
insurance_data_3 <- insurance_data %>%
  mutate(
    charges = as.numeric(log(charges))
  )

### NEGATIVE Y-VARIABLE IS NOT OKAY FOR GAMMA FAMILY!!!!! So dont scale

insur_3_split <- initial_split(insurance_data_3)

insur_3_train <- training(insur_3_split)

############################################
# Model Prep
glm_rec <- recipe(charges ~., data = insur_3_train) %>% 
  step_dummy(all_nominal_predictors())

glm_spec <- linear_reg() %>% 
  set_mode("regression") %>% 
  set_engine('glm', family = Gamma(link = 'inverse'))

# link = 'log' - 
# link = 'inverse' - For very skewed data
# link = 'identity' - If the relationship is close to linear

glm_wf <- workflow() %>%
  add_recipe(glm_rec) %>%
  add_model(glm_spec)


################################################
# Fitting Data

glm_fit <- glm_wf %>%
  last_fit(insur_3_split)


################################################
# Results

glm_fit %>% collect_metrics()

glm_fit %>% collect_predictions()

# un-doing log function on predictions (back to $)
predicted_dollars <- exp(predictions)

# Result Viz
c <- glm_fit %>%
  collect_predictions() %>% 
  ggplot(aes(charges, .pred)) +
  geom_point(alpha = .5) +
  geom_abline(color = 'red', size = 1) +
  labs(
    title = "Actual vs Predicted (GLM Regression)",
    x = "Charges",
    y = "Predicted"
  ) +
  theme_minimal()


d <- glm_fit %>%
  collect_predictions() %>%
  mutate(residual = charges - .pred) %>%
  ggplot(aes(.pred, residual)) +
  geom_point(alpha = 0.5) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "red") +
  labs(
    title = "Residuals vs Predicted (GLM Regression)",
    x = "Predicted Charges",
    y = "Residuals"
  ) +
  theme_minimal()


################################################
# Summary for GLM

# Getting Summary
fitted_glm_workflow <- glm_fit$.workflow[[1]]

# Full model summary
fitted_glm_model <- fitted_glm_workflow %>%
  extract_fit_engine()

summary(fitted_glm_model)


################################################
# LR vs GLM
# Combine plots
(final_plot <- (a + b) /
  (c + d))












