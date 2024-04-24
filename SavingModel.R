# Saving the best logistic regression model
saveRDS(logistic_model$finalModel, "./models/logistic_model.rds")

# Load the saved model
loaded_logistic_model <- readRDS("./models/logistic_model.rds")

# Prepare new data for prediction (using Pima Indians dataset variables)
new_data <- data.frame(
  Pregnancies = 5,
  Glucose = 130,
  BloodPressure = 70,
  SkinThickness = 30,
  Insulin = 50,
  BMI = 25,
  DiabetesPedigreeFunction = 0.5,
  Age = 40
)

# Use the loaded model to make predictions
predictions_loaded_model <- predict(loaded_logistic_model, newdata = new_data)

# Print predictions
print(predictions_loaded_model)
