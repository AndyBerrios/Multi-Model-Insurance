
############################################
# data prep 
insurance_split  <- initial_split(insurance_data, strata = charges)

insurance_train <- training(insurance_split)
insurance_test <- testing(insurance_split)


############################################
# model prep 
rf_rec <- recipe(charges ~ ., data = insurance_train) %>%
  step_dummy(all_nominal_predictors())  # Convert categorical to dummy variables

rf_spec <- rand_forest(
  mtry = tune(),
  trees = 1000,
  min_n = tune(),
) %>% 
  set_mode("regression") %>% 
  set_engine("ranger", importance = "permutation")


rf_wf <- workflow() %>% 
  add_recipe(rf_rec) %>% 
  add_model(rf_spec) 

############################################
# train model 
insurance_folds <- vfold_cv(insurance_train, strata = charges)

doParallel::registerDoParallel()
set.seed(123)

# tune_results
rf_res <- tune_grid(
  rf_wf,
  resamples = insurance_folds,
  grid = 20,
  metrics = metric_set(rmse, rsq)
)

# Use rmse for regression (Root Mean Squared Error)
# Use roc_auc for classification

rf_res %>% 
  collect_metrics() %>% 
  filter(.metric == 'rmse') %>% 
  select(mean, min_n, mtry) %>% 
  pivot_longer(min_n:mtry,
               values_to = 'value',
               names_to = 'parameter') %>% 
  ggplot(aes(value, mean, color = parameter)) + 
  geom_point(show.legend = FALSE) +
  facet_wrap(~parameter, scales = 'free_x')

show_best(rf_res)

############################################
# model 2

rf_grid <- grid_regular(
  mtry(range = c(6,10)),
  min_n(range = c(20,40)),
  levels = 5
)


# model 2 results 
set.seed(234)
rf_res_2 <- tune_grid(
  rf_wf,
  resamples = insurance_folds,
  grid = rf_grid
) # the tune in the rf_spec receive the grid values

show_best(rf_res_2)


#  model 2 viz
rf_res_2 %>% 
  collect_metrics() %>% 
  filter(.metric == 'rmse') %>% 
  mutate(min_n = factor(min_n)) %>% 
  ggplot(aes(mtry, mean, color = min_n)) + 
  geom_line(alpha = .5, size = 1.5) +
  geom_point()




############################################
# Final test model
best_rmse <- select_best(rf_res_2, metric = "rmse")


final_rf <- finalize_workflow(
  rf_wf,     # workflow, not model spec
  best_rmse    # best hyperparameters
)
# A finalized workflow
#   It’s not just the model — it’s the full modeling pipeline:
#   Your data cleaning (recipe) step
#   Your trained Random Forest model with tuned settings
#   Everything locked in and ready to fit or predict.


library(vip)
# VIP only after final model is finalized
final_rf %>%
  fit(data = insurance_train) %>%
  vip(geom = "point")


# final results
final_res <- final_rf %>% 
  last_fit(insurance_split)

final_res %>% 
  collect_metrics()
# The Random Forest model has a Root Mean Squared Error (RMSE) of about $4,405  and an R² of 85.4%, meaning it 
# explains most of the variation in insurance charges. This is a strong and reliable predictive model.


final_res %>% 
  collect_predictions()

final_res %>% 
  collect_predictions() %>%
  ggplot(aes(x = charges, y = .pred)) +
  geom_point(alpha = 0.6) +  # scatter points
  geom_abline(slope = 1, intercept = 0, color = "red", linetype = "dashed") +  # ideal prediction line
  labs(
    title = "Actual vs Predicted Insurance Charges",
    x = "Actual Charges",
    y = "Predicted Charges"
  ) +
  theme_minimal()

# Predictions track closely with actual charges, with only some minor underestimation for mid-range charges. 
# No major systematic bias detected.

final_res %>% 
  collect_predictions() %>%
  mutate(residual = charges - .pred) %>%
  ggplot(aes(x = .pred, y = residual)) +
  geom_point(alpha = 0.6) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "red") +
  labs(
    title = "Residuals vs Predicted Charges",
    x = "Predicted Charges",
    y = "Residual (Actual - Predicted)"
  ) +
  theme_minimal()

# The residuals are mostly randomly distributed around zero, with no clear patterns or trends, suggesting that the 
# Random Forest model provides unbiased predictions. While some underprediction is observed for very high 
# insurance charges, overall model performance remains strong.

