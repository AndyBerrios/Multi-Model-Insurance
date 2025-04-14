############################################
# data prep 
insurance_data_2 <- insurance_data %>%
  mutate(
    charges = as.numeric(scale(charges))
  )

insur_2_split <- initial_split(insurance_data_2)

insur_2_train <- training(insur_2_split)

############################################
# Model Prep
glm_rec <- recipe(charges ~., data = insur_2_train) %>% 
  step_dummy(all_nominal_predictors())

glm_spec <- linear_reg() %>% 
  set_mode("regression") %>% 
  set_engine('glm', family = )

glm_wf <- workflow() %>%
  add_recipe(glm_rec) %>%
  add_model(glm_spec)


################################################
# Fitting Data

glm_fit <- glm_wf %>%
  last_fit(insurance_split)


################################################
# Results

glm_fit %>% collect_metrics()

glm_fit %>% collect_predictions()


# Result Viz
glm_fit %>%
  collect_predictions() %>% 
  ggplot(aes(charges, .pred)) +
  geom_point(alpha = .5) +
  geom_abline(color = 'red', size = 1)


# Getting Summary
fitted_glm_workflow <- glm_fit$.workflow[[1]]

# Full model summary
fitted_glm_model <- fitted_glm_workflow %>%
  extract_fit_engine()

summary(fitted_glm_model)













