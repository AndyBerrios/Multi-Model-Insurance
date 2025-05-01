library(tictoc)

# Linear Model
time_lm <- system.time({
  lm_fit <- lm_wf %>% last_fit(insurance_split)
})

# GLM
time_glm <- system.time({
  glm_fit <- glm_wf %>% last_fit(insurance_split)
})

# Random Forest
time_rf <- system.time({
  final_res <- final_rf %>% last_fit(insurance_split)
})

# XGBoost
time_xgb <- system.time({
  final_xgb_fit <- final_xgb_wf %>% last_fit(insurance_split)
})

# View timing results
time_lm
time_glm
time_rf
time_xgb