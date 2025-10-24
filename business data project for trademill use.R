library(tidyverse)
library(ggplot2)
library(dplyr)

# Load the data
data <- read.csv("CardioGoodFitness.csv")

# Glimpse at the data
glimpse(data)
summary(data)
str(data)

#Questions to answer:
#How many rows and columns?
# What are the variable types?
#Any missing values?
#What are the unique values in 'Product'?
#2. Data Preprocessing and Cleaning

#Objective: Check for issues like:
#Missing values
#Data types
#Outliers
# Check missing values
colSums(is.na(data))

# Convert categorical variables to factors
data$Product <- as.factor(data$Product)
data$Gender <- as.factor(data$Gender)
data$MaritalStatus <- as.factor(data$MaritalStatus)

# Univariate Analysis
#Objective: Understand the distribution of each variable.
#Plots to create:
#Histograms (Age, Income, Miles, Usage, Fitness)
#Bar plots (Product, Gender, Marital Status)
# Example: Product Distribution
ggplot(data, aes(x = Product)) + geom_bar(fill = "steelblue") + ggtitle("Treadmill Model Distribution")

# Histogram of Income
ggplot(data, aes(x = Income)) + geom_histogram(binwidth = 10000, fill = "skyblue", color = "black") + ggtitle("Distribution of Income")
#Summary Stats:
# Average Age, Income, Education

data %>%
  summarise(
    avg_age = mean(Age),
    avg_income = mean(Income),
    avg_education = mean(Education)
  )
#4. Bivariate and Multivariate Analysis
#Objective: Investigate how customer characteristics relate to the treadmill model purchased.
# A. Income vs Product

ggplot(data, aes(x = Product, y = Income)) + 
  geom_boxplot(fill = "lightgreen") + 
  ggtitle("Income by Treadmill Model")
#B. Age vs Product
ggplot(data, aes(x = Product, y = Age)) + 
  geom_boxplot(fill = "lightblue") + 
  ggtitle("Age by Treadmill Model")
#C. Gender vs Product
ggplot(data, aes(x = Product, fill = Gender)) + 
  geom_bar(position = "dodge") + 
  ggtitle("Gender Distribution across Models")
#D. Marital Status vs Product
ggplot(data, aes(x = Product, fill = MaritalStatus)) + 
  geom_bar(position = "dodge") + 
  ggtitle("Marital Status by Treadmill Model")
#E. Fitness Score by Product
ggplot(data, aes(x = Product, y = Fitness)) + 
  geom_boxplot(fill = "coral") + 
  ggtitle("Self-rated Fitness by Product")
#5. Statistical Testing
table(data$Product, data$Gender)
chisq.test(table(data$Product, data$Gender))
#6.➤ B. Does Age Differ Between Models?
anova_age <- aov(Age ~ Product, data = data)
summary(anova_age)
#7➤ C. Does Income Differ by Product?
anova_income <- aov(Income ~ Product, data = data)
summary(anova_income)
#6. Customer Profile for Each Product

#Summarize characteristics by Product:
data %>%
  group_by(Product) %>%
  summarise(
    avg_age = mean(Age),
    avg_income = mean(Income),
    avg_miles = mean(Miles),
    avg_usage = mean(Usage),
    avg_fitness = mean(Fitness),
    avg_education = mean(Education),
    count = n()
  )

png("fig_income_by_product.png", width=800, height=600) 

boxplot(Income ~ Product, data=df, main="Income by Product", ylab="Income") 

dev.off() 