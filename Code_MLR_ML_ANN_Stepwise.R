## Load the necessary libraries
library(dplyr)
library(tidyr)
library(caret)
library(car)

# Load the dataset
data <- read.csv("C:\\Users\\hp\\Desktop\\Training\\Training\\Dara_code_multivatre analysyis\\reg.csv")
data <- na.omit(data)

# Split the data into independent and dependent variables
X <- data %>% select(RH, Tmax, Tmin)
y <- data$Yield

# Check for multicollinearity
cor(X) # Check correlation matrix

# Check VIF values
vif(lm(Tmax ~ RH + Tmin, data = data))
vif(lm(Tmin ~ RH + Tmax, data = data))
vif(lm(RH ~ Tmax + Tmin, data = data))
# If any variables are highly correlated (e.g., VIF > 5), remove them
#X <- X %>% select(-Wind_Speed)
# Split the data into training and testing sets
# Split the data into training and testing sets
set.seed(123) # Set random seed for reproducibility
train_indices <- createDataPartition(y, p = 0.7, list = FALSE)
train_data <- data[train_indices, ]
test_data <- data[-train_indices, ]

train_data
test_data 

# Build the regression model
model <- lm(Yield ~ RH + Tmax + Tmin, data = train_data)
summary(model)

# Evaluate the model using the testing set
predictions <- predict(model, newdata = test_data)
predictions

r_squared <- cor(predictions, test_data$Yield) ^ 2
MAE <- mean(abs(predictions - test_data$Yield))
RMSE <- sqrt(mean((predictions - test_data$Yield) ^ 2))

# Print the evaluation metrics
print(paste0("R-squared: ", round(r_squared, 2)))
print(paste0("MAE: ", round(MAE, 2)))
print(paste0("RMSE: ", round(RMSE, 2)))

################################################# Plot#################################################
library(ggplot2)

# Create a data frame with the observed and predicted values
df <- data.frame(observed = test_data$Yield, predicted = predictions)

# Plot the observed vs predicted values
ggplot(df, aes(x = observed, y = predicted)) +
  geom_point() +
  geom_smooth(method = "lm", color = "red") +
  labs(x = "Observed Yield", y = "Predicted Yield",
       title = "Observed vs Predicted Yield") +
  theme_bw() +
  theme(plot.title = element_text(hjust = 0.5))

###############################################Randomforest###########################

library(randomForest)

# Build the random forest model using the train_data data frame
model <- randomForest(Yield ~ RH + Tmax + Tmin, data = train_data)

# Print a summary of the model
print(model)

# Evaluate the model using the testing set
predictions <- predict(model, newdata = test_data)
predictions

r_squared <- cor(predictions, test_data$Yield) ^ 2
MAE <- mean(abs(predictions - test_data$Yield))
RMSE <- sqrt(mean((predictions - test_data$Yield) ^ 2))

# Print the evaluation metrics
print(paste0("R-squared: ", round(r_squared, 2)))
print(paste0("MAE: ", round(MAE, 2)))
print(paste0("RMSE: ", round(RMSE, 2)))


################################################# Plot#################################################
library(ggplot2)

# Create a data frame with the observed and predicted values
df <- data.frame(observed = test_data$Yield, predicted = predictions)

# Plot the observed vs predicted values
ggplot(df, aes(x = observed, y = predicted)) +
  geom_point() +
  geom_smooth(method = "lm", color = "red") +
  labs(x = "Observed Yield", y = "Predicted Yield",
       title = "Observed vs Predicted Yield") +
  theme_bw() +
  theme(plot.title = element_text(hjust = 0.5))


############################################### Suport vector model#################################

# Load the e1071 package
library(e1071)

# Build the SVM model using the train_data data frame
model <- svm(Yield ~ RH + Tmax + Tmin, data = train_data)

# Print a summary of the model
print(model)

# Evaluate the model using the testing set
predictions <- predict(model, newdata = test_data)
predictions

r_squared <- cor(predictions, test_data$Yield) ^ 2
MAE <- mean(abs(predictions - test_data$Yield))
RMSE <- sqrt(mean((predictions - test_data$Yield) ^ 2))

# Print the evaluation metrics
print(paste0("R-squared: ", round(r_squared, 2)))
print(paste0("MAE: ", round(MAE, 2)))
print(paste0("RMSE: ", round(RMSE, 2)))

################################################# Plot#################################################
library(ggplot2)

# Create a data frame with the observed and predicted values
df <- data.frame(observed = test_data$Yield, predicted = predictions)

# Plot the observed vs predicted values
ggplot(df, aes(x = observed, y = predicted)) +
  geom_point() +
  geom_smooth(method = "lm", color = "red") +
  labs(x = "Observed Yield", y = "Predicted Yield",
       title = "Observed vs Predicted Yield") +
  theme_bw() +
  theme(plot.title = element_text(hjust = 0.5))


############################################### ANN #################################

# Load the neuralnet package
library(neuralnet)

# Create a formula for the model
formula <- Yield ~ RH + Tmax + Tmin

# Create a data frame for the model
model_data <- cbind(train_data["Yield"], train_data[c("RH", "Tmax", "Tmin")])

# Build the ANN model
model <- neuralnet(formula, data = model_data, hidden = 2)

# Print a summary of the model
print(model)

# Evaluate the model using the testing set
predictions <- predict(model, newdata = test_data)
predictions

r_squared <- cor(predictions, test_data$Yield) ^ 2
MAE <- mean(abs(predictions - test_data$Yield))
RMSE <- sqrt(mean((predictions - test_data$Yield) ^ 2))

# Print the evaluation metrics
print(paste0("R-squared: ", round(r_squared, 2)))
print(paste0("MAE: ", round(MAE, 2)))
print(paste0("RMSE: ", round(RMSE, 2)))


############################################### Stepwise #################################
library(dplyr)
library(tidyr)
library(caret)
library(car)

# Load the dataset
data <- read.csv("J:\\Class_DU\\Dara_code_multivatre analysyis\\reg.csv")

# Split the data into independent and dependent variables
X <- data %>% select(RH, Tmax, Tmin)
y <- data$Yield

# Check for multicollinearity
cor(X) # Check correlation matrix
#vif(X) # Check VIF values

# Split the data into training and testing sets
set.seed(123) # Set random seed for reproducibility
train_indices <- createDataPartition(y, p = 0.7, list = FALSE)
train_data <- data[train_indices, ]
test_data <- data[-train_indices, ]

# Perform stepwise multiple linear regression
model <- lm(Yield ~ 1, data = train_data) # Start with an intercept-only model
step_model <- step(model, direction = "both", scope = list(lower = ~ 1, upper = ~ RH + Tmax + Tmin), data = train_data)
summary(step_model)

# Evaluate the model using the testing set
predictions <- predict(step_model, newdata = test_data)
r_squared <- cor(predictions, test_data$Yield) ^ 2
MAE <- mean(abs(predictions - test_data$Yield))
RMSE <- sqrt(mean((predictions - test_data$Yield) ^ 2))

# Print the evaluation metrics
print(paste0("R-squared: ", round(r_squared, 2)))
print(paste0("MAE: ", round(MAE, 2)))
print(paste0("RMSE: ", round(RMSE, 2)))

################################################ Foward########################################



# Load the dataset
data <- read.csv("J:\\Class_DU\\Dara_code_multivatre analysyis\\reg.csv")

# Split the data into independent and dependent variables
X <- data[, c("RH", "Tmax", "Tmin")]
y <- data$Yield

# Split the data into training and testing sets
set.seed(123)
train_indices <- createDataPartition(y, p = 0.7, list = FALSE)
train_data <- data[train_indices, ]
test_data <- data[-train_indices, ]

# Build an empty model object
model <- lm(Yield ~ 1, data = train_data)

# Create an empty vector to store selected predictors
selected_vars <- character()

# Iterate until all predictors have been selected
while (length(selected_vars) < ncol(X)) {
  # Initialize variables
  best_r_squared <- 0
  best_var <- ""
  
  # Loop through remaining predictors
  for (var in colnames(X)[!colnames(X) %in% selected_vars]) {
    # Build a model with the selected predictors and the current predictor
    temp_model <- lm(paste0("Yield ~ ", paste0(c(selected_vars, var), collapse = " + ")), data = train_data)
    # Calculate R-squared for the model
    temp_r_squared <- summary(temp_model)$r.squared
    
    # Check if the current predictor improves the model
    if (temp_r_squared > best_r_squared) {
      best_r_squared <- temp_r_squared
      best_var <- var
    }
  }
  
  # Add the best predictor to the selected list
  selected_vars <- c(selected_vars, best_var)
  
  # Update the model with the selected predictors
  formula_str <- paste0("Yield ~ ", paste0(selected_vars, collapse = " + "))
  model <- update(model, formula_str)
  
  # Print the selected predictors and their R-squared values
  print(paste0("Selected: ", paste(selected_vars, collapse = ", ")))
  print(paste0("R-squared: ", round(best_r_squared, 2)))
}

# Print the final model summary
summary(model)

# Evaluate the model using the testing set
predictions <- predict(model, newdata = test_data)
r_squared <- cor(predictions, test_data$Yield) ^ 2
MAE <- mean(abs(predictions - test_data$Yield))
RMSE <- sqrt(mean((predictions - test_data$Yield) ^ 2))

# Print the evaluation metrics
print(paste0("R-squared: ", round(r_squared, 2)))
print(paste0("MAE: ", round(MAE, 2)))
print(paste0("RMSE: ", round(RMSE, 2)))
