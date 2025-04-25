############################################

# scaling is ONLY FOR PREDICTORS

insurance_split <- initial_split(insurance_data)

insurance_train <- training(insurance_split)

################################################
# Model Prep
set.seed(123)

# scaling predictors
lm_rec <- recipe(charges ~ ., data = insurance_train) %>%
  step_dummy(all_nominal_predictors()) %>%
  step_normalize(all_numeric_predictors())  # ✅ Not outcome


lm_spec <- linear_reg() %>% 
  set_mode('regression') %>% 
  set_engine('lm')

lm_wf <- workflow() %>%
  add_recipe(lm_rec) %>%
  add_model(lm_spec)

################################################
# Fitting Data
lm_fit <- lm_wf %>% 
  last_fit(insur_2_split)

################################################
# Results 
lm_fit %>% collect_metrics()

lm_fit %>% collect_predictions()


# Result Viz
a <- lm_fit %>% 
  collect_predictions() %>% 
  ggplot(aes(charges, .pred)) +
  geom_point(alpha = .6) +
  geom_abline(slope = 1, intercept = 0, color = "red", linetype = "dashed") +
  labs(
    title = "Charges vs Predicted (Linear Regression)",
    x = "Charges",
    y = "Predicted"
  ) +
  theme_minimal()
  

b <- lm_fit %>%
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



################################################
# Summary for LM
# Model Summary

# Extract the fitted workflow from last_fit
fitted_workflow <- lm_fit$.workflow[[1]]

# Pull out the fitted model object
full_lm <- fitted_workflow %>%
  extract_fit_engine()

# View full model summary (r-squared, p-values, coefficients, etc.)
summary(full_lm)
