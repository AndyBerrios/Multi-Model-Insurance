# BRING TEST TRAIN DATA

model <- lm(charges ~ ., data = insurance_data)

summary(model)

################################################
# only significant values
model2 <- lm(charges ~ children + smoker + bmi + age, data = insurance_data)

summary(model2)
