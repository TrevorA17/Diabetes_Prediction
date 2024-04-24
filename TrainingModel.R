# Load dataset
pima_data <- read.csv("data/diabetes.csv", colClasses = c(
  Pregnancies = "numeric",
  Glucose = "numeric",
  BloodPressure = "numeric",
  SkinThickness = "numeric",
  Insulin = "numeric",
  BMI = "numeric",
  DiabetesPedigreeFunction = "numeric",
  Age = "numeric",
  Outcome = "factor"
), header = TRUE)

# Display the structure of the dataset
str(pima_data)

# View the first few rows of the dataset
head(pima_data)

# Open the dataset in a viewer window
View(pima_data)

# Load necessary libraries
library(caret)

# Set seed for reproducibility
set.seed(123)

# Split the data into 70% training and 30% testing
train_indices <- createDataPartition(pima_data$Outcome, p = 0.7, list = FALSE)

# Create training and testing sets
train_data <- pima_data[train_indices, ]
test_data <- pima_data[-train_indices, ]

# Display the dimensions of the training and testing sets
cat("Training data dimensions:", dim(train_data), "\n")
cat("Testing data dimensions:", dim(test_data), "\n")

# Load necessary libraries
library(boot)

# Define the function to compute the statistic of interest (mean glucose level)
compute_statistic <- function(data, indices) {
  sample_data <- data[indices, ]
  mean_glucose <- mean(sample_data$Glucose, na.rm = TRUE)
  return(mean_glucose)
}

# Set the number of bootstrap replicates
num_replicates <- 1000

# Perform bootstrapping
bootstrapped_means <- boot(data = pima_data, statistic = compute_statistic, R = num_replicates)

# Display the bootstrapped mean glucose levels
print(bootstrapped_means)

# Load necessary libraries
library(caret)

# Define the control parameters for cross-validation
ctrl <- trainControl(method = "cv",    # Use k-fold cross-validation
                     number = 10)      # Specify the number of folds (e.g., 10-fold)

# Define the predictive model (e.g., linear regression)
model <- train(Outcome ~ .,                # Specify the formula for the model
               data = pima_data,          # Specify the dataset
               method = "glm",            # Specify the modeling method (e.g., generalized linear model)
               trControl = ctrl)          # Specify the control parameters for cross-validation

# Display the cross-validation results
print(model)


# Load necessary libraries
library(caret)

# Define the control parameters for model training
ctrl <- trainControl(method = "cv",   # Use k-fold cross-validation
                     number = 10)     # Specify the number of folds (e.g., 10-fold)

# Train logistic regression model
logistic_model <- train(Outcome ~ .,     # Specify the formula for the model
                        data = pima_data,   # Specify the dataset
                        method = "glm",     # Specify the modeling method (logistic regression)
                        trControl = ctrl)   # Specify the control parameters for cross-validation

# Display the trained logistic regression model
print(logistic_model)

# Train decision tree model
decision_tree_model <- train(Outcome ~ .,       # Specify the formula for the model
                             data = pima_data, # Specify the dataset
                             method = "rpart", # Specify the modeling method (decision trees)
                             trControl = ctrl) # Specify the control parameters for cross-validation

# Display the trained decision tree model
print(decision_tree_model)

# Train SVM model
svm_model <- train(Outcome ~ .,           # Specify the formula for the model
                   data = pima_data,     # Specify the dataset
                   method = "svmRadial", # Specify the modeling method (SVM with radial kernel)
                   trControl = ctrl)     # Specify the control parameters for cross-validation

# Display the trained SVM model
print(svm_model)

