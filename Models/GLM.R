
library(statmod) # for tweedie
############################################
# Model Prep
set.seed(123)


glm_rec <- recipe(charges ~., data = insur_3_train) %>% 
  step_dummy(all_nominal_predictors()) %>% 
  step_normalize(all_numeric_predictors())

glm_spec <- linear_reg() %>%
  set_engine("glm", family = tweedie(var.power = .8, link.power = 0)) %>% 
  set_mode('regression')


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







################################################
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












