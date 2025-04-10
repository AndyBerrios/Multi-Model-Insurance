# BRING TEST TRAIN DATA

############################################
# data prep 
insurance_data_2 <- insurance_data %>%
  mutate(
    charges = as.numeric(scale(charges))
  )





################################################


model <- lm(charges ~ ., data = insurance_data)

summary(model)

################################################
# only significant values
model2 <- lm(charges ~ children + smoker + bmi + age, data = insurance_data)

summary(model2)
