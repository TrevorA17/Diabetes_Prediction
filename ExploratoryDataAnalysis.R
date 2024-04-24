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

# Compute measures of central tendency
central_tendency <- sapply(pima_data[, c("Pregnancies", "Glucose", "BloodPressure", "SkinThickness", "Insulin", "BMI", "DiabetesPedigreeFunction", "Age")], 
                           function(x) c(Mean = mean(x, na.rm = TRUE), 
                                         Median = median(x, na.rm = TRUE), 
                                         Mode = names(sort(table(x), decreasing = TRUE)[1])))

# Display measures of central tendency
print("Measures of Central Tendency:")
print(central_tendency)

# Compute measures of distribution
distribution <- sapply(pima_data[, c("Pregnancies", "Glucose", "BloodPressure", "SkinThickness", "Insulin", "BMI", "DiabetesPedigreeFunction", "Age")], 
                       function(x) c(Range = diff(range(x, na.rm = TRUE)), 
                                     Variance = var(x, na.rm = TRUE), 
                                     Standard_Deviation = sd(x, na.rm = TRUE)))

# Display measures of distribution
print("Measures of Distribution:")
print(distribution)

# Compute measures of relationship
correlation_matrix <- cor(pima_data[, c("Pregnancies", "Glucose", "BloodPressure", "SkinThickness", "Insulin", "BMI", "DiabetesPedigreeFunction", "Age")], 
                          use = "pairwise.complete.obs")

# Display correlation matrix
print("Correlation Matrix:")
print(correlation_matrix)

# Perform ANOVA test
anova_results <- aov(BMI ~ Age, data = pima_data)

# Display ANOVA results
print("ANOVA Results:")
print(summary(anova_results))

# Load necessary libraries
library(ggplot2)

# Univariate plots for numerical variables
# Histograms
histograms <- lapply(pima_data[, c("Pregnancies", "Glucose", "BloodPressure", "SkinThickness", "Insulin", "BMI", "DiabetesPedigreeFunction", "Age")], 
                     function(x) ggplot(pima_data, aes(x)) + geom_histogram(binwidth = 5) + labs(x = names(x)))

# Boxplots
boxplots <- lapply(pima_data[, c("Pregnancies", "Glucose", "BloodPressure", "SkinThickness", "Insulin", "BMI", "DiabetesPedigreeFunction", "Age")], 
                   function(x) ggplot(pima_data, aes(y = x)) + geom_boxplot() + labs(y = names(x)))

# Density plots
density_plots <- lapply(pima_data[, c("Pregnancies", "Glucose", "BloodPressure", "SkinThickness", "Insulin", "BMI", "DiabetesPedigreeFunction", "Age")], 
                        function(x) ggplot(pima_data, aes(x)) + geom_density() + labs(x = names(x)))

# Display plots
print("Histograms:")
print(histograms)
print("Boxplots:")
print(boxplots)
print("Density Plots:")
print(density_plots)
