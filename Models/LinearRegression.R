# BRING TEST TRAIN DATA

############################################
# data prep 
insurance_data_2 <- insurance_data %>%
  mutate(
    charges = as.numeric(scale(charges))
  )

insur_2_split <- initial_split(insurance_data_2)

insur_2_train <- training(insur_2_split)

################################################
# LM Prep
lm_rec <- recipe(charges ~., data = insur_2_train) %>% 
  step_dummy(all_nominal_predictors())

# declare model
lm_spec <- linear_reg() %>% 
  set_mode('regression') %>% 
  set_engine('lm')

lm_wf <- workflow() %>%
  add_recipe(lm_rec) %>%
  add_model(lm_spec)

lm_fit <- lm_wf %>% 
  last_fit(insur_2_split)

################################################
# Results 
lm_fit %>% collect_metrics()

lm_fit %>% collect_predictions()

# Result Analysis
lm_fit %>% 
  collect_predictions() %>% 
  ggplot(aes(charges, .pred)) +
  geom_point(alpha = .6) +
  geom_abline(slope = 1, intercept = 0, color = "red", linetype = "dashed")

lm_fit %>%
  collect_predictions() %>%
  mutate(residual = charges - .pred) %>%
  ggplot(aes(.pred, residual)) +
  geom_point(alpha = 0.5) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "red") +
  labs(
    title = "Residuals vs Predicted (Linear Regression)",
    x = "Predicted Charges",
    y = "Residuals"
  ) +
  theme_minimal()





# Model Summary
# Extract the fitted workflow from last_fit
fitted_workflow <- lm_fit$.workflow[[1]]

# Pull out the fitted model object
full_lm <- fitted_workflow %>%
  extract_fit_engine()

# View full model summary (r-squared, p-values, coefficients, etc.)
summary(full_lm)
