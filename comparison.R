# Assuming you've already run collect_metrics() on each
glm_metrics <- glm_fit %>% collect_metrics() %>% mutate(model = "GLM")
lm_metrics <- lm_fit %>% collect_metrics() %>% mutate(model = "LM")
rf_metrics <- final_res %>% collect_metrics() %>% mutate(model = "Random Forest")
xgb_metrics <- final_xgb_fit %>% collect_metrics() %>% mutate(model = "XGBoost")

# Combine into one table
all_metrics <- bind_rows(glm_metrics, lm_metrics, rf_metrics, xgb_metrics) %>%
  select(model, .metric, .estimate)

