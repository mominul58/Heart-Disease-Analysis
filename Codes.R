# Load necessary library
library(readr)
library(tidyverse)
library(dplyr)

# URL of the raw CSV file
url <- "https://raw.githubusercontent.com/mominul58/Heart-Disease-Analysis/main/heart.csv"

# Read the CSV file directly from the URL
dataset <- read_csv(url)

# View basic structure and summary of the dataset
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

print("Frequency Table for Sex:")
print(sex_frequency)

print("Frequency Table for Age Groups:")
print(age_frequency)

print("Frequency Table for Chest Pain Types (cp):")
print(cp_frequency)



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
print("Cholesterol Stats by Chest Pain Type:")
print(cholesterol_stats)


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
print("Max Heart Rate Stats by Exercise Induced Angina:")
print(max_heart_stats)

#### 4. Descriptive statistics of resting blood pressure by age groups####


# Create 5-year age groups
dataset <- dataset %>%
  mutate(age_group = cut(age, breaks = seq(floor(min(age, na.rm = TRUE)), 
                                           ceiling(max(age, na.rm = TRUE)) + 5, 
                                           by = 5), 
                         include.lowest = TRUE))

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

print("Resting Blood Pressure Stats by Age Group:")
print(bp_stats_by_age)
