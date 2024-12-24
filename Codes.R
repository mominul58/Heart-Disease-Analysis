# Project-1 
# Load necessary library
library(readr)
library(tidyverse)
library(dplyr)
library(openxlsx)

#Directory for data save
setwd('F:/R Programming/Public Health')
getwd()

# I use csv data from my github repository

# URL of the raw CSV file
url <- "https://raw.githubusercontent.com/mominul58/Heart-Disease-Analysis/main/heart.csv"

# Read the CSV file directly from the URL
dataset <- read_csv(url)

# basic structure and summary of the dataset
str(dataset)
summary(dataset)


#### 1. Missing Profiling and Demographic Frequency tables####

# Missing values profiling
missing_profile <- dataset %>% summarise_all(~sum(is.na(.)))
print("Missing Profile:")
print(missing_profile)

# Frequency tables for demographic variables (assuming 'sex', 'age', and 'cp' are demographics)
sex_frequency <- table(dataset$sex)
age_frequency <- dataset %>% count(cut_width(age, width = 10))
cp_frequency <- table(dataset$cp)

print(sex_frequency)

print(age_frequency)
#write.xlsx(age_frequency,file="age_frequency.xlsx") #i save this file in my directory

print(cp_frequency)
#write.xlsx(cp_frequency,file="cp_frequency.xlsx")


#### 2. Descriptive statistics of serum cholesterol by chest pain type ####
cholesterol_stats <- dataset %>% 
  group_by(cp) %>% 
  summarise(
    Mean = mean(chol, na.rm = TRUE),
    Median = median(chol, na.rm = TRUE),
    SD = sd(chol, na.rm = TRUE),
    Q1 = quantile(chol, 0.25, na.rm = TRUE),
    Q3 = quantile(chol, 0.75, na.rm = TRUE)
  )

print(cholesterol_stats)
#write.xlsx(cholesterol_stats,file="cholesterol_stats.xlsx")

#### 3. Descriptive statistics of maximum heart rate achieved by exercise induced angina ####

max_heart_stats <- dataset %>% 
  group_by(exang) %>% 
  summarise(
    Mean = mean(thalach, na.rm = TRUE),
    Median = median(thalach, na.rm = TRUE),
    SD = sd(thalach, na.rm = TRUE),
    Q1 = quantile(thalach, 0.25, na.rm = TRUE),
    Q3 = quantile(thalach, 0.75, na.rm = TRUE)
  )

print(max_heart_stats)
#write.xlsx(max_heart_stats,file="max_heart_stats.xlsx")

#### 4. Descriptive statistics of resting blood pressure by age groups####

# Create 5-year age groups
dataset$age_group <- cut(dataset$age, 
                         breaks = seq(min(dataset$age, na.rm = TRUE), 
                                      max(dataset$age, na.rm = TRUE) + 5, 
                                      by = 5), 
                         include.lowest = TRUE)


# Calculate descriptive statistics for resting blood pressure (trestbps) by 5-year age group
bp_stats_by_age <- dataset %>%
  group_by(age_group) %>%
  summarise(
    Mean = mean(trestbps, na.rm = TRUE),
    Median = median(trestbps, na.rm = TRUE),
    SD = sd(trestbps, na.rm = TRUE),
    Q1 = quantile(trestbps, 0.25, na.rm = TRUE),
    Q3 = quantile(trestbps, 0.75, na.rm = TRUE),
    .groups = 'drop'
  )


print(bp_stats_by_age)
#write.xlsx(bp_stats_by_age,file="bp_stats_by_age.xlsx")

#### Extra task ####
# 3 more extra task i added in my project
#### 5. Perform correlation analysis between continuous variables like age, trestbps,chol, and thalach. Which variables show the strongest relationship? ####


# Load necessary libraries
#install.packages("corrplot")
library(corrplot)  # For visualizing the correlation matrix


# Select continuous variables for correlation analysis
selected_vars <- dataset %>% 
  select(age, trestbps, chol, thalach)

# Compute the correlation matrix
cor_matrix <- cor(selected_vars, use = "complete.obs")  # use complete.obs to exclude NAs

# Display the correlation matrix
print(cor_matrix)
#write.xlsx(cor_matrix,file="cor_matrix.xlsx")

# Visualize the correlation matrix using corrplot
corrplot(cor_matrix, method = "color", type = "upper", 
         addCoef.col = "black", # Add correlation coefficients
         tl.col = "blue",       # Color for labels
         tl.srt = 45,           # Rotate text labels
         diag = FALSE)          # Hide diagonal



#### 6. Use scatter plots to visualize the relationship between cholesterol (chol) and age (age) ####

ggplot(dataset, aes(x = age, y = chol)) +
  geom_point(aes(color = factor(target)), size = 2) +  # Color points by heart disease status (target)
  labs(title = "Cholesterol vs Age",
       x = "Age",
       y = "Cholesterol Level") +
  theme_minimal() +
  theme(legend.title = element_blank())  # Remove legend title




#### 7. Create pie charts and bar plots to compare The proportion of males and females with heart disease.####

ggplot(dataset, aes(x = factor(sex), fill = factor(target))) +
  geom_bar(position = "fill") +  # Use "fill" to show proportions
  labs(title = "Proportion of Males and Females with Heart Disease",
       x = "Gender (1 = Male, 0 = Female)",
       y = "Proportion",
       fill = "Heart Disease") +
  scale_x_discrete(labels = c("0" = "Female", "1" = "Male")) +
  scale_fill_manual(values = c("lightblue", "salmon"),
                    labels = c("No Heart Disease", "Heart Disease")) +
  theme_minimal()
