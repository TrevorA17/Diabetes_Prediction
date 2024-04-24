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

# Compute measures of frequency
outcome_frequency <- table(pima_data$Outcome)
outcome_percentage <- prop.table(outcome_frequency) * 100

# Display measures of frequency
print("Frequency of Outcome:")
print(outcome_frequency)
print("Percentage of Outcome:")
print(outcome_percentage)

