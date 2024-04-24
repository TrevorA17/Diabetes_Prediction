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

# Function to generate individual plots
generate_plots <- function(variable_name, data) {
  # Histogram
  hist_plot <- ggplot(data, aes(x = .data[[variable_name]])) +
    geom_histogram(binwidth = 5, fill = "skyblue", color = "black") +
    labs(title = paste("Histogram of", variable_name),
         x = variable_name, y = "Frequency")
  
  # Boxplot
  box_plot <- ggplot(data, aes(y = .data[[variable_name]])) +
    geom_boxplot(fill = "lightgreen", color = "black") +
    labs(title = paste("Boxplot of", variable_name),
         x = "", y = variable_name)
  
  # Density plot
  density_plot <- ggplot(data, aes(x = .data[[variable_name]])) +
    geom_density(fill = "pink", color = "black") +
    labs(title = paste("Density Plot of", variable_name),
         x = variable_name, y = "Density")
  
  # Output each plot
  print(hist_plot)
  print(box_plot)
  print(density_plot)
}

# Generate and output plots for each numerical variable
for (variable_name in c("Pregnancies", "Glucose", "BloodPressure", "SkinThickness", "Insulin", "BMI", "DiabetesPedigreeFunction", "Age")) {
  print(paste("Plots for", variable_name))
  generate_plots(variable_name, pima_data)
}

# Load necessary libraries
library(ggplot2)

# Scatter plot for Pregnancies vs. Glucose
scatter_plot_1 <- ggplot(pima_data, aes(x = Pregnancies, y = Glucose)) +
  geom_point(color = "blue", alpha = 0.5) +
  labs(title = "Scatter Plot of Pregnancies vs. Glucose", x = "Pregnancies", y = "Glucose")

print(scatter_plot_1)

# Scatter plot for Pregnancies vs. BloodPressure
scatter_plot_2 <- ggplot(pima_data, aes(x = Pregnancies, y = BloodPressure)) +
  geom_point(color = "blue", alpha = 0.5) +
  labs(title = "Scatter Plot of Pregnancies vs. BloodPressure", x = "Pregnancies", y = "BloodPressure")

print(scatter_plot_2)

# Scatter plot for Pregnancies vs. SkinThickness
scatter_plot_3 <- ggplot(pima_data, aes(x = Pregnancies, y = SkinThickness)) +
  geom_point(color = "blue", alpha = 0.5) +
  labs(title = "Scatter Plot of Pregnancies vs. SkinThickness", x = "Pregnancies", y = "SkinThickness")

print(scatter_plot_3)

# Scatter plot for Pregnancies vs. Insulin
scatter_plot_4 <- ggplot(pima_data, aes(x = Pregnancies, y = Insulin)) +
  geom_point(color = "blue", alpha = 0.5) +
  labs(title = "Scatter Plot of Pregnancies vs. Insulin", x = "Pregnancies", y = "Insulin")

print(scatter_plot_4)

# Scatter plot for Pregnancies vs. BMI
scatter_plot_5 <- ggplot(pima_data, aes(x = Pregnancies, y = BMI)) +
  geom_point(color = "blue", alpha = 0.5) +
  labs(title = "Scatter Plot of Pregnancies vs. BMI", x = "Pregnancies", y = "BMI")

print(scatter_plot_5)

# Scatter plot for Pregnancies vs. DiabetesPedigreeFunction
scatter_plot_6 <- ggplot(pima_data, aes(x = Pregnancies, y = DiabetesPedigreeFunction)) +
  geom_point(color = "blue", alpha = 0.5) +
  labs(title = "Scatter Plot of Pregnancies vs. DiabetesPedigreeFunction", x = "Pregnancies", y = "DiabetesPedigreeFunction")

print(scatter_plot_6)

# Scatter plot for Pregnancies vs. Age
scatter_plot_7 <- ggplot(pima_data, aes(x = Pregnancies, y = Age)) +
  geom_point(color = "blue", alpha = 0.5) +
  labs(title = "Scatter Plot of Pregnancies vs. Age", x = "Pregnancies", y = "Age")

print(scatter_plot_7)

# Scatter plot for Glucose vs. BloodPressure
scatter_plot_8 <- ggplot(pima_data, aes(x = Glucose, y = BloodPressure)) +
  geom_point(color = "blue", alpha = 0.5) +
  labs(title = "Scatter Plot of Glucose vs. BloodPressure", x = "Glucose", y = "BloodPressure")

print(scatter_plot_8)

# Scatter plot for Glucose vs. SkinThickness
scatter_plot_9 <- ggplot(pima_data, aes(x = Glucose, y = SkinThickness)) +
  geom_point(color = "blue", alpha = 0.5) +
  labs(title = "Scatter Plot of Glucose vs. SkinThickness", x = "Glucose", y = "SkinThickness")

print(scatter_plot_9)

# Scatter plot for Glucose vs. Insulin
scatter_plot_10 <- ggplot(pima_data, aes(x = Glucose, y = Insulin)) +
  geom_point(color = "blue", alpha = 0.5) +
  labs(title = "Scatter Plot of Glucose vs. Insulin", x = "Glucose", y = "Insulin")

print(scatter_plot_10)

# Scatter plot for Glucose vs. BMI
scatter_plot_11 <- ggplot(pima_data, aes(x = Glucose, y = BMI)) +
  geom_point(color = "blue", alpha = 0.5) +
  labs(title = "Scatter Plot of Glucose vs. BMI", x = "Glucose", y = "BMI")

print(scatter_plot_11)

# Scatter plot for Glucose vs. DiabetesPedigreeFunction
scatter_plot_12 <- ggplot(pima_data, aes(x = Glucose, y = DiabetesPedigreeFunction)) +
  geom_point(color = "blue", alpha = 0.5) +
  labs(title = "Scatter Plot of Glucose vs. DiabetesPedigreeFunction", x = "Glucose", y = "DiabetesPedigreeFunction")

print(scatter_plot_12)

# Scatter plot for Glucose vs. Age
scatter_plot_13 <- ggplot(pima_data, aes(x = Glucose, y = Age)) +
  geom_point(color = "blue", alpha = 0.5) +
  labs(title = "Scatter Plot of Glucose vs. Age", x = "Glucose", y = "Age")

print(scatter_plot_13)


