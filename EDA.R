library(tidyverse)
library(GGally) # for pair-plot
library(naniar) # to check NAs
library(tidymodels) # for all Models
library(tune) # selecting best
library(xgboost) # still needed even with Tidy Models 
library(patchwork) # needed to 'facet' different visualization together
library(ggridges)



############################################
# reading data
library(here)
insurance_data <- read.csv(here("insurance.csv"))


############################################
# pre-processing / data integrity

# NA Handeling
any_na(insurance_data)

# Checking for Outliers
# 1. Calculate IQR bounds
Q1 <- quantile(insurance_data$charges, 0.25)
Q3 <- quantile(insurance_data$charges, 0.75)
IQR_val <- IQR(insurance_data$charges)

lower_bound <- Q1 - 1.5 * IQR_val
upper_bound <- Q3 + 1.5 * IQR_val

# 2. Filter out the outliers (optional)
no_outliers <- insurance_data %>%
  filter(charges >= lower_bound & charges <= upper_bound)

# 3. Count how many were outliers
num_outliers <- nrow(insurance_data) - nrow(no_outliers)

cat("Total outliers:", num_outliers, "\n")


############################################
# feature-engineering

# str(insurance_data)

insurance_data <- insurance_data %>%
  mutate(
    sex = factor(sex),
    smoker = factor(smoker),
    region = factor(region)
  )


############################################
# checking normality of data

dist_plot <- insurance_data %>% 
  ggplot(aes(charges)) +
  geom_histogram(binwidth = 500) + 
  xlab('Charges in USD') + 
  ylab('Number of Individuals') +
  theme(axis.title.x = element_text(size = 20),
        axis.title.y = element_text(size = 20))


# qqnorm(insurance_data$charges, main = "Q-Q Plot of Charges")
# qqline(insurance_data$charges, col = "red", lwd = 2)
 


# qqnorm(sqrt(insurance_data$charges), main = "Q-Q Plot of Charges")
# qqline(sqrt(insurance_data$charges), col = "red", lwd = 2)



############################################
# plotting begins - PAIR PLOTS ARE MOSTLY FOR YOU

# Pair Plot to Explore Relationships Between Variables
pair_plots <- insurance_data %>%
  mutate(smoker = factor(smoker, levels = c("no", "yes"))) %>%  # force order
  ggpairs(aes(color = smoker, alpha = 0.3)) +
  scale_color_manual(values = c("blue", "red")) +   # force blue for "no", red for "yes"
  labs(title = "Pair Plot of Key Variables") +
  theme(
    axis.text.x = element_text(angle = 90, hjust = 1, size = 5),
    axis.text.y = element_text(hjust = 1, size = 7)
  )



pair_plot_2 <- insurance_data %>% 
  select(age, bmi, smoker, charges) %>% 
  ggpairs(aes(color = smoker, alpha = .4)) +
  theme_minimal()


# Value | Strength | Interpretation
# +0.90 to +1.00 | Very strong | As one variable increases, the other does too (linear).
# +0.70 to +0.89 | Strong | Clear upward trend with some spread.
# +0.50 to +0.69 | Moderate | Trend is visible but weaker.
# +0.30 to +0.49 | Weak | Some relationship, but noisy.
# +0.00 to +0.29 | Very weak | Essentially no linear relationship.

#############################################

age_charges <- insurance_data %>% 
  mutate(age_category = factor(case_when(
    age < 40 ~ 'young',
    age < 55 ~ 'mid_age',
    TRUE ~ 'older'), 
    levels =c('young', 'mid_age', 'older'))) %>% 
  group_by(age_category) %>% 
  summarise(avg_cost = mean(charges)) %>% 
  ggplot(aes(x = age_category, y = avg_cost, fill = age_category)) + 
  geom_bar(stat = 'identity') +
  labs(subtitle = "
       Young: <40 years, 
       Mid Age: 40-54 years, 
       Older: 55+ years",
       x = "Age Category",
       y = "Average Cost",
       fill = "Age Group") +
  theme(axis.title.x = element_text(size = 0),
        axis.title.y = element_text(size = 20),
        legend.position = 'none')

#############################################

age_charges_2 <- insurance_data %>% ggplot(aes(x = age, y = charges, color = age)) +
  geom_jitter(width = 3, alpha = .5) +
  labs(title = "Age vs. Insurance Charges", x = "Age", y = "Charges")

#############################################

gender_charges <- insurance_data %>%
  ggplot(aes(x = sex, y = charges, fill = sex))+
  geom_violin(alpha = .7) +
       ylab('Charges') + 
  theme(axis.text.x  = element_text(size = 20),
        axis.title.y = element_text(size = 20),
        axis.title.x = element_text(size = 0),
        legend.position = 'none')

#############################################

bmi_charges <- insurance_data %>% 
  ggplot(aes(x = bmi, y = charges)) + 
  geom_smooth(size = 2) +
  labs(title = "Relationship Between BMI and Health Insurance Charges",
       subtitle = 'Quantiles of BMI Relating to Cost',
       x = "BMI",
       y = "Charges",
       colour = "BMI")

#############################################

children_charges <- insurance_data %>% 
  ggplot(aes(x = charges , y = factor(children), fill = factor(children))) +
  geom_density_ridges(alpha = .7) +
       xlab('Charges') +
       ylab('Number of Children') +
  theme(axis.title.x = element_text(size = 20),
        axis.title.y = element_text(size = 20),
        legend.position = 'top')

#############################################

smoker_charges <- insurance_data %>% 
  ggplot(aes(x = charges, fill = smoker)) +
  geom_histogram(binwidth = 1000, alpha = .7, position = 'identity') + 
       xlab("Charges in USD") + 
       ylab("Number of Individuals") +  
  theme(axis.title.x = element_text(size = 20),
        axis.title.y = element_text(size = 20), 
        legend.position = 'top')
                                                                                                               

#############################################

region_chareges <- insurance_data %>% 
  ggplot(aes(x = region, y = charges)) +
  geom_jitter(aes(color = region), alpha = .3, size = 1) + # jitter first
  geom_boxplot(alpha = .3) +
  labs(title = "Insurance Charges by Region", x = "Region", y = "Charges")

#############################################
# ANOVA

region_anova <- aov(charges ~ region, data = insurance_data)
summary(region_anova)
TukeyHSD(region_anova)

children_anova <- aov(children ~ region, data = insurance_data)
summary(children_anova)

TukeyHSD(children_anova)


#############################################
# post model plots
 chrg_plot <- insurance_data %>% 
  mutate(weight_category = case_when(
    bmi < 18.5 ~ 'underweight', 
    bmi < 24.9 ~ 'normal',
    bmi < 29.9 ~ 'overweight',
    bmi >= 30 ~ 'obese'
  )) %>% 
  ggplot(aes(x = age, y = charges, color = weight_category)) +
  geom_point(alpha = 0.5) + 
  facet_wrap(~smoker)


















