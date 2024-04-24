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
