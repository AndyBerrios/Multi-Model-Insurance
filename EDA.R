library(tidyverse)
library(GGally) # for pair-plot
library(naniar) # to check NAs
library(tidymodels) # for all Models
library(tune) # selecting best
library(xgboost) # still needed even with Tidy Models 



############################################
# reading data
insurance_data <- read.csv('insurance.csv')

############################################
# pre-processing

# NA Handeling
any_na(insurance_data)

# Checking for Outliers


############################################
# feature-engineering

str(insurance_data)

insurance_data <- insurance_data %>%
  mutate(
    sex = factor(sex),
    smoker = factor(smoker),
    region = factor(region),
    children = factor(children)
  )


############################################
# checking normality of data

insurance_data %>% 
  ggplot(aes(charges)) +
  geom_histogram(binwidth = 500)

qqnorm(insurance_data$charges, main = "Q-Q Plot of Charges")
qqline(insurance_data$charges, col = "red", lwd = 2)


qqnorm(sqrt(insurance_data$charges), main = "Q-Q Plot of Charges")
qqline(sqrt(insurance_data$charges), col = "red", lwd = 2)


############################################
# plotting begins

# Pair Plot to Explore Relationships Between Variables
insurance_data %>%
  mutate(smoker = factor(smoker, levels = c("no", "yes"))) %>%  # force order
  ggpairs(aes(color = smoker, alpha = 0.3)) +
  scale_color_manual(values = c("blue", "red")) +   # force blue for "no", red for "yes"
  labs(title = "Pair Plot of Key Variables") +
  theme(
    axis.text.x = element_text(angle = 90, hjust = 1, size = 5),
    axis.text.y = element_text(hjust = 1, size = 7)
  )



insurance_data %>% 
  select(age, bmi, smoker, charges) %>% 
  ggpairs(aes(color = smoker, alpha = .4)) +
  theme_minimal()


#############################################

insurance_data %>% 
  mutate(age_category = factor(case_when(
    age < 40 ~ 'young',
    age < 55 ~ 'mid_age',
    TRUE ~ 'older'), 
    levels =c('young', 'mid_age', 'older'))) %>% 
  group_by(age_category) %>% 
  summarise(avg_cost = mean(charges)) %>% 
  ggplot(aes(x = age_category, y = avg_cost, fill = age_category)) + 
  geom_bar(stat = 'identity') +
  labs(title = 'Plot of Charges based on Age Catergory',
       subtitle = "
       Young: <40 years, 
       Mid Age: 40-54 years, 
       Older: 55+ years",
       x = "Age Category",
       y = "Average Cost",
       fill = "Age Group")

#############################################

insurance_data %>% ggplot(aes(x = age, y = charges, color = age)) +
  geom_jitter(width = 3, alpha = .5) +
  labs(title = "Age vs. Insurance Charges", x = "Age", y = "Charges")

#############################################

insurance_data %>%
  ggplot(aes(x = sex, y = charges, fill = sex))+
  geom_violin(alpha = .7) + 
  labs(title = 'Cost of Insurance',
       subtitle = 'Men vs Women',
       x = 'Sex',
       y = 'Charges',
       fill = 'Sex')

#############################################

insurance_data %>% 
  ggplot(aes(x = bmi, y = charges)) + 
  geom_smooth(size = 2) +
  labs(title = "Relationship Between BMI and Health Insurance Charges",
       subtitle = 'Quantiles of BMI Relating to Cost',
       x = "BMI",
       y = "Charges",
       colour = "BMI")

#############################################

insurance_data %>% 
  ggplot(aes(x = charges , y = factor(children), fill = factor(children))) +
  geom_density_ridges(alpha = .7) + 
  labs(title = 'Distribution of Charges',
       subtitle = 'with Children',
       x = 'Charges',
       y = 'Number of Children', 
       fill = 'Children Count')

#############################################

insurance_data %>% 
  ggplot(aes(x = charges, fill = smoker)) +
  geom_histogram(binwidth = 1000, alpha = .7, position = 'identity') +
  labs(title = "Distribution of Annual Insurance Charges", x = "Charges", y = "Frequency")

#############################################

insurance_data %>% 
  ggplot(aes(x = region, y = charges)) +
  geom_jitter(aes(color = region), alpha = .3, size = 1) + # jitter first
  geom_boxplot(alpha = .3) +
  labs(title = "Insurance Charges by Region", x = "Region", y = "Charges")



