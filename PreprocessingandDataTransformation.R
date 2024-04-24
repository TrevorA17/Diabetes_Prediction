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

# Check for missing values
missing_values <- colSums(is.na(pima_data))

# Display columns with missing values
print("Columns with missing values:")
print(missing_values[missing_values > 0])

# Load necessary libraries
library(dplyr)

# Function to normalize data
normalize_data <- function(data) {
  normalized_data <- data %>%
    mutate_if(is.numeric, function(x) (x - min(x, na.rm = TRUE)) / (max(x, na.rm = TRUE) - min(x, na.rm = TRUE)))
  return(normalized_data)
}

# Apply normalization to numerical variables
normalized_pima_data <- normalize_data(pima_data)

# Display the first few rows of the normalized dataset
head(normalized_pima_data)
