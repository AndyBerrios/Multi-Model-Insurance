############################################
# Data prep 
insurance_split <- initial_split(insurance_data, strata = charges)
insurance_train <- training(insurance_split)
insurance_test  <- testing(insurance_split)

############################################
# Model Prep
xgb_rec <- recipe(charges ~ ., data = insurance_train) %>% 
  step_dummy(all_nominal_predictors())

xgb_spec <- boost_tree(
  trees = 1000,             # Total number of trees
  tree_depth = tune(),      # Max depth of each tree 
  learn_rate = tune(),      # Learning rate 
  loss_reduction = tune(),  # Minimum loss reduction (gamma) 
  sample_size = tune(),     # Subsample ratio of training instances 
  mtry = tune()             # Number of features to consider at each split
) %>%
  set_mode("regression") %>%
  set_engine("xgboost")

xgb_wf <- workflow() %>% 
  add_recipe(xgb_rec) %>% 
  add_model(xgb_spec)

############################################
# Cross validation
insurance_folds <- vfold_cv(insurance_train, 
                            v = 5, 
                            strata = charges)

############################################
# Model 1
doParallel::registerDoParallel()
set.seed(123)

# (rmse) Root-mean-square deviation
xgb_res <- tune_grid(
  xgb_wf,
  resamples = insurance_folds,
  grid = 20,  # Random grid search
  metrics = metric_set(rmse, rsq)
)


############################################
# Final model
best_xgb <- select_best(xgb_res, metric = 'rmse')

final_xgb_wf <- finalize_workflow(
  xgb_wf,
  best_xgb
)

############################################
# Fitting Data
final_xgb_fit <- final_xgb_wf %>%
  last_fit(insurance_split)

############################################
# result analysis

final_xgb_fit %>% collect_metrics()

final_xgb_fit %>% collect_predictions()

final_xgb_fit %>% 
  collect_predictions %>% 
  ggplot(aes(x = charges, y = .pred)) +
  geom_point(alpha = .5) + 
  geom_abline(color = 'red', size = 1)


# residuals
final_xgb_fit %>% 
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


############################################
# VIP
final_xgb_fit$.workflow[[1]] %>%
  extract_fit_parsnip() %>%
  vip(geom = "point")
























