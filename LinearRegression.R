insurance_data <- insurance_data %>%
  mutate(
    sex = ifelse(sex == "female", 1, 0),
    smoker = ifelse(smoker == "yes", 1, 0),
    region = factor(region)
  )

insurance_data <- model.matrix(~ . -1, data = insurance_data) %>%
  as.data.frame()

model <- lm(charges ~ ., data = insurance_data)

summary(model)

################################################

model2 <- lm(charges ~ children + smoker + bmi + age, data = insurance_data)

summary(model2)
