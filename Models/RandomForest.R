
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


############################################
# train model 1 prep 
tune_spec <- rand_forest(
  mtry = tune(),
  trees = 1000,
  min_n = tune(),
) %>% 
  set_mode("regression") %>% 
  set_engine("ranger")

tune_wf <- workflow() %>% 
  add_recipe(insur_rec) %>% 
  add_model(tune_spec) 

############################################
# train model 1 running 
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

############################################
# model 1 results

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

############################################
# model 2 results 

set.seed(234)
regular_res <- tune_grid(
  tune_wf,
  resamples = insurance_folds,
  grid = rf_grid
)

show_best(regular_res)

############################################
# train model 2 viz

regular_res %>% 
  collect_metrics() %>% 
  filter(.metric == 'rmse') %>% 
  mutate(min_n = factor(min_n)) %>% 
  ggplot(aes(mtry, mean, color = min_n)) + 
  geom_line(alpha = .5, size = 1.5) +
  geom_point()
















