#Statistical Malaria Modelling (Using Surveilance Data)
#ARIMA Time Series Forecasting for Malaria Cases
library(forecast) # for forecasting
library(tseries) # Time series and models

# Simulated malaria case data
set.seed(123) # For reproducibility
#Converting to time series 100 random poisson distributed values 
# The  monthly data that starts from 2015
malaria_cases <- ts(rpois(100, lambda = 50), frequency = 12, start = c(2015, 1))

# Fit ARIMA model
fit <- auto.arima(malaria_cases)

# Forecast for next 12 months
forecasted_cases <- forecast(fit, h = 12)
library(ggplot2) # Graphics and data visualization
# Plot the forecast
autoplot(forecasted_cases) + 
  ggtitle("Malaria Cases Forecast") +
  theme_minimal()
#Modelling using Machine Learning Approach
#Predicting Malaria Cases using Random Forest
library(randomForest) # Implements Random Forest Algorithm 
library(caret) # Classification and Regression Trains

# Simulated dataset of various mean and standard deviations
set.seed(42) # Reproducibility
# Generate a dataframe that is distributed randomly
data <- data.frame(
  temperature = rnorm(100, 27, 2),
  rainfall = rnorm(100, 150, 30),
  humidity = rnorm(100, 75, 5),
  cases = rpois(100, lambda = 50)
)

#Split data into training and tests using stratified sampling
#80% of data allocated to training set,20% to test set into matrix
trainIndex <- createDataPartition(data$cases, p = 0.8, list = FALSE)
# Create training set by selecting rows that match the trainIndex
trainData <- data[trainIndex,]
# Create test set by selecting all rows NOT in trainIndex
testData <- data[-trainIndex,]

# Train model by using 100 trees 
rf_model <- randomForest(cases ~ ., data = trainData, ntree = 100)

# Predict on test data
predictions <- predict(rf_model, testData)

# Evaluate Model
cor(predictions, testData$cases)
#Model Performance Metrics
#Metrics for Regression, timeseries 
library(Metrics) # Implements metrics for supervised learning
#Root Mean Squared Error to evaluate Model performance
rmse_val <- rmse(testData$cases, predictions)
r2_val <- cor(testData$cases, predictions)^2
# Display fo the results
cat("RMSE:", rmse_val, "\nR-squared:", r2_val)
#HyperParameter Tuning For better Accuracy
# the best 'mtry' value by increasing it in steps of 1.5 times the previous value.
tuned_rf <- tuneRF(trainData[, -4], trainData$cases, stepFactor = 1.5, improve = 0.01)
#############
#Epidemiological Analysis
# Load necessary libraries
library(dplyr) # structuring the data
library(ggplot2) # Visualizatio and graphics
library(epitools) # Eppidemiological operations

# Load a sample dataset (Simulated epidemiological data)
set.seed(123) # Reproducibility
n <- 1000 # Generate 1000 random normal numbers
epidemiology_data <- data.frame(
  id = 1:n,
  age = sample(20:80, n, replace = TRUE),
  sex = sample(c("Male", "Female"), n, replace = TRUE),
  exposure = sample(c("Yes", "No"), n, replace = TRUE, prob = c(0.3, 0.7)),
  disease_status = sample(c(1, 0), n, replace = TRUE, prob = c(0.2, 0.8))
)
# Compute incidence and prevalence
total_population <- nrow(epidemiology_data)
total_cases <- sum(epidemiology_data$disease_status == 1)
prevalence_rate <- (total_cases / total_population) * 100

cat("Prevalence Rate: ", round(prevalence_rate, 2), "%\n")
# Stratified incidence by exposure status
table_exposure_disease <- table(epidemiology_data$exposure, epidemiology_data$disease_status)
incidence_rate_exposed <- (table_exposure_disease["Yes", "1"] / sum(table_exposure_disease["Yes", ])) * 100
incidence_rate_unexposed <- (table_exposure_disease["No", "1"] / sum(table_exposure_disease["No", ])) * 100

cat("Incidence Rate in Exposed: ", round(incidence_rate_exposed, 2), "%\n")
cat("Incidence Rate in Unexposed: ", round(incidence_rate_unexposed, 2), "%\n")
# Compute Odds Ratio
odds_ratio <- oddsratio(table_exposure_disease)
print(odds_ratio) # Display results

# Logistic Regression: Assessing risk factors
logistic_model <- glm(disease_status ~ age + sex + exposure, data = epidemiology_data, family = binomial)
summary(logistic_model)
# Visualization: Disease Prevalence by Age Group
ggplot(epidemiology_data, aes(x = age, fill = factor(disease_status))) +
  geom_histogram(binwidth = 5, position = "fill") +
  labs(title = "Disease Prevalence by Age Group", x = "Age", y = "Proportion", fill = "Disease Status") +
  theme_minimal()
#####
# Summarize data: Compute prevalence per age group
summary_data <- epidemiology_data %>%
  group_by(age) %>%
  summarize(prevalence = mean(disease_status))  # Mean gives proportion of disease cases

# Line plot for prevalence by age
ggplot(summary_data, aes(x = age, y = prevalence)) +
  geom_line(color = "blue", size = 1) +
  labs(title = "Disease Prevalence by Age Group",
       x = "Age", y = "Prevalence Rate") +
  theme_minimal()
