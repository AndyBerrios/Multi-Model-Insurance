
############################################
# data prep 
insurance_split  <- initial_split(insurance_data, strata = charges)

insurance_train <- training(insurance_split)
insurance_test <- testing(insurance_split)


############################################
# model prep 
insur_rec <- recipe(charges ~ ., data = insurance_train) %>%
  step_dummy(all_nominal_predictors())  # Convert categorical to dummy variables

insur_prep <- prep(insur_rec)
juiced <- juice(insur_prep)


tune_spec <- rand_forest(
  mtry = tune(),
  trees = 1000,
  min_n = tune(),
) %>% 
  set_mode("regression") %>% 
  set_engine("ranger", importance = "permutation")

# ---------------------- Variable Importance Types ----------------------
#
# "impurity" (default)
#   - Measures how much each feature decreases the impurity (e.g., Gini or variance) at each tree split.
#   - Higher decrease = more important.
#   - Fast to calculate.
#   - ⚠️ Can be biased toward variables with more categories or wide ranges.
#
# "permutation"
#   - Measures increase in prediction error when a feature’s values are randomly shuffled.
#   - If shuffling hurts model performance a lot, the feature is important.
#   - More reliable and less biased than impurity.
#   - Slightly slower to calculate.
#
# "none"
#   - No variable importance is calculated.
#   - Model will train faster, but you can't plot VIP or measure feature importance.
#
# ---------------------- Common Setting for Random Forest ----------------------
#
# Best practice: use "permutation" if you want more reliable variable importance, 
# otherwise "impurity" is fine for quick analysis.
#
# Example:
# set_engine("ranger", importance = "permutation")

tune_wf <- workflow() %>% 
  add_recipe(insur_rec) %>% 
  add_model(tune_spec) 

############################################
# train model 
insurance_folds <- vfold_cv(insurance_train, strata = charges)

doParallel::registerDoParallel()
set.seed(123)

# tune_results
tune_res <- tune_grid(
  tune_wf,
  resamples = insurance_folds,
  grid = 20,
  metrics = metric_set(rmse, rsq)
)

# Use rmse for regression
# Use roc_auc for regression

tune_res %>% 
  collect_metrics() %>% 
  filter(.metric == 'rmse') %>% 
  select(mean, min_n, mtry) %>% 
  pivot_longer(min_n:mtry,
               values_to = 'value',
               names_to = 'parameter') %>% 
  ggplot(aes(value, mean, color = parameter)) + 
  geom_point(show.legend = FALSE) +
  facet_wrap(~parameter, scales = 'free_x')

show_best(tune_res)

############################################
# model 2

rf_grid <- grid_regular(
  mtry(range = c(5,10)),
  min_n(range = c(26,34)),
  levels = 5
)


# model 2 results 
set.seed(234)
regular_res <- tune_grid(
  tune_wf,
  resamples = insurance_folds,
  grid = rf_grid
)

show_best(regular_res)


#  model 2 viz
regular_res %>% 
  collect_metrics() %>% 
  filter(.metric == 'rmse') %>% 
  mutate(min_n = factor(min_n)) %>% 
  ggplot(aes(mtry, mean, color = min_n)) + 
  geom_line(alpha = .5, size = 1.5) +
  geom_point()


############################################
# Final test model
best_rmse <- select_best(regular_res, metric = "rmse")

final_rf <- finalize_workflow(
  tune_wf,     # workflow, not model spec
  best_rmse    # best hyperparameters
)


library(vip)

final_rf %>%
  fit(data = insurance_train) %>%
  vip(geom = "point")














