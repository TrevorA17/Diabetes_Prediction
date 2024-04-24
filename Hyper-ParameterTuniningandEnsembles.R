
# Load necessary libraries
library(caret)

# Define the control parameters for model training
ctrl <- trainControl(method = "cv",    # Use k-fold cross-validation
                     number = 10)      # Specify the number of folds (e.g., 10-fold)

# Define the search grid for random search
random_grid <- expand.grid(C = c(0.01, 0.1, 1, 10, 100),   # Specify the range of values for the regularization parameter (C)
                           kernel = c("linear", "radial")) # Specify the types of kernels

# Perform random search for SVM
svm_random_search <- train(Outcome ~ .,           # Specify the formula for the model
                           data = pima_data,     # Specify the dataset
                           method = "svmRadial", # Specify the modeling method (SVM with radial kernel)
                           trControl = ctrl,     # Specify the control parameters for cross-validation
                           tuneGrid = random_grid) # Use the defined search grid

# Display the best model from random search
print(svm_random_search)

# Load necessary libraries
library(caret)

# Define the control parameters for model training
ctrl <- trainControl(method = "cv",    # Use k-fold cross-validation
                     number = 10)      # Specify the number of folds (e.g., 10-fold)

# Train bagged decision tree model
bagging_model <- train(Outcome ~ .,            # Specify the formula for the model
                       data = pima_data,       # Specify the dataset
                       method = "treebag",    # Specify the modeling method (bagged decision trees)
                       trControl = ctrl)      # Specify the control parameters for cross-validation

# Display the bagged decision tree model
print(bagging_model)


# Define the base learners
base_learners <- c("glm", "rpart", "svmRadial")



