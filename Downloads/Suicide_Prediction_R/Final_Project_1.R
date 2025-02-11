ls -l ~/Downloads | grep "Suicide_Prediction_R"
# Step 1: Define the research question
# In this case, we assume the research question is predicting suicide rates based on socio-economic factors.

# Step 2: Data Pre-processing and Cleaning

# Load necessary libraries
library(tidyverse)
library(reshape2)

# Load the data
who_suicide_statistics <- read_csv("Downloads/who_suicide_statistics.csv")
head(who_suicide_statistics)
# Handle missing values
who_suicide_statistics$suicides_no[is.na(who_suicide_statistics$suicides_no)] <- 0
# Handle missing values in the response variable 'suicides_no'
model_data <- model_data %>%
  filter(!is.na(suicides_no))
# Remove duplicates
who_suicide_statistics <- who_suicide_statistics %>%
  distinct()

# Convert 'year' to a factor
who_suicide_statistics$year <- as.factor(who_suicide_statistics$year)

# Explore and handle outliers
numeric_columns <- who_suicide_statistics %>%
  select(suicides_no, population)

# Identify and handle outliers
who_suicide_statistics <- who_suicide_statistics %>%
  filter(!(suicides_no > quantile(suicides_no, 0.75) + 1.5 * IQR(suicides_no) |
             suicides_no < quantile(suicides_no, 0.25) - 1.5 * IQR(suicides_no)))

# Handle categorical variables
who_suicide_statistics$sex <- as.factor(who_suicide_statistics$sex)
who_suicide_statistics$age <- as.factor(who_suicide_statistics$age)

#Final check
summary(who_suicide_statistics)

# Prepare data for multiple linear regression
# Assuming 'suicides_no' as the response variable and 'population' and 'year' as predictors
# Adjust predictors based on your research question

model_data <- who_suicide_statistics %>%
  select(suicides_no, population, year)

model_data
# Encode 'year' as numeric for regression (you can use other encoding methods)
model_data$year <- as.numeric(model_data$year)


# Step 3: EDA (Exploratory Data Analysis)
# Visualize relationships between variables using ggplot2

# Box Plots by Categories: Visualize the distribution of numeric variables across different categories.

ggplot(who_suicide_statistics, aes(x = sex, y = suicides_no, fill = sex)) +
  geom_boxplot() +
  labs(title = "Box Plot of Suicides by Sex", x = "Sex", y = "Suicides_no")

# Heatmap: Visualize the relationship between multiple variables using a heatmap.

ggplot(who_suicide_statistics, aes(x = sex, y = age, fill = suicides_no)) +
  geom_tile() +
  labs(title = "Heatmap of Suicides by Sex and Age Group", x = "Sex", y = "Age", fill = "Suicides_no")



# Split the data into training and testing sets
set.seed(123)  # for reproducibility
train_indices <- sample(1:nrow(model_data), 0.8 * nrow(model_data))
train_data <- model_data[train_indices, ]
test_data <- model_data[-train_indices, ]

# Fit the multiple linear regression model on the training set
lm_model <- lm(suicides_no ~ ., data = train_data)

# View summary of the model
summary(lm_model)

# Model Evaluation
# Assess the model fit and performance using diagnostic plots
# For example, residuals vs. fitted values, normal Q-Q plot
par(mfrow = c(2, 2))  # Set up a 2x2 grid for plots
plot(lm_model)       # Plot diagnostics

# Feature Engineering
# Consider creating additional features or interactions between features to improve model performance
# For example, let's create an interaction term between 'population' and 'year'
model_data$interaction_term <- model_data$population * model_data$year

# Cross-Validation
# Consider using cross-validation to assess the model's performance on different subsets of the data
library(caret)
set.seed(123)  # Set a seed for reproducibility
cv_results <- train(
  suicides_no ~ .,
  data = model_data,
  method = "lm",
  trControl = trainControl(method = "cv", number = 5)
)
print(cv_results)

# Feature Selection
# If you have a large number of predictors, consider using methods for feature selection
# For example, let's use the 'step' function for backward elimination
lm_model_step <- step(lm_model)

# View the summary of the model after feature selection
summary(lm_model_step)

