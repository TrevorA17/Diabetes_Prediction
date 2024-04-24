# Load the saved logistic regression model
loaded_logistic_model <- readRDS("./models/best_logistic_model.rds")

#* @apiTitle Diabetes Prediction Model API
#* @apiDescription Used to predict diabetes onset.

#* @param Pregnancies Number of times pregnant
#* @param Glucose Plasma glucose concentration a 2 hours in an oral glucose tolerance test
#* @param BloodPressure Diastolic blood pressure (mm Hg)
#* @param SkinThickness Triceps skin fold thickness (mm)
#* @param Insulin 2-Hour serum insulin (mu U/ml)
#* @param BMI Body mass index (weight in kg/(height in m)^2)
#* @param DiabetesPedigreeFunction Diabetes pedigree function
#* @param Age Age (years)

#* @get /diabetes_prediction

predict_diabetes <- function(Pregnancies, Glucose, BloodPressure, SkinThickness,
                             Insulin, BMI, DiabetesPedigreeFunction, Age) {
  
  # Create a data frame using the arguments
  to_be_predicted <- data.frame(
    Pregnancies = as.numeric(Pregnancies),
    Glucose = as.numeric(Glucose),
    BloodPressure = as.numeric(BloodPressure),
    SkinThickness = as.numeric(SkinThickness),
    Insulin = as.numeric(Insulin),
    BMI = as.numeric(BMI),
    DiabetesPedigreeFunction = as.numeric(DiabetesPedigreeFunction),
    Age = as.numeric(Age)
  )
  
  # Use the loaded model to make predictions
  prediction <- predict(loaded_logistic_model, newdata = to_be_predicted)
  
  # Return the prediction
  return(prediction)
}
