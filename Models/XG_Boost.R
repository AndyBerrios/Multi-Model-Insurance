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















