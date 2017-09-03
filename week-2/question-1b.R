# Clear the enviroment
rm(list = ls())

# Comment in set.seed(111) to repeat results across different values of k
set.seed(111) 

# Load the kknn library
require('kknn')

# Load credit card data into a data frame
data_df <- read.table("credit_card_data-headers.txt", header=TRUE)

# Randomly identify 70% of data points to use for creation of training_df
sample70 <- sample(nrow(data_df), round(nrow(data_df)*.7))

# Create training (70pct of records)
training_df <- data_df[sample70, ]

# Remaining dataset (30pct of records) after creation of training_df - this will be split into validation and test
remaining_df <- data_df[-sample70, ]

# Randomly identify 50% of data points to use for creation of validation_df from remaining_df
sample50 <- sample(nrow(remaining_df), round(nrow(remaining_df)*.5))

# Create validation_df and test_df (each 15pct of original data_df) from remaining_df
validation_df <- remaining_df[sample50, ]
test_df <- remaining_df[-sample50, ]

### Create a function that trains a model with neighbors as input and returns predictions
train.model <- function(neighbors){
  # train the model using training_df, validation_df, and neighbors value
  model <- kknn(R1 ~., training_df, validation_df, k = neighbors, scale = TRUE)
  
  # store the predictions
  model_fitted <- as.matrix(model$fitted.values)
  
  # round the fitted values
  model_fitted_rounded <- as.matrix(lapply(model_fitted[, 1], round))
  
  # merge rounded yhat values back into model_fitted
  model_results <- data.frame(validation_df[, 'R1'], model_fitted_rounded)
  
  # update column names
  colnames(model_results) <- c("actual", "predicted")
  
  # return the results
  return(model_results)
}

### Create a function that calculates the accuracy based on predictions
eval.model <- function(model){
  # instantiates a results vector of length nrow(model)
  results <- vector("list", nrow(model))  
  
  # loops through each record to check that actual == predicted
  for(i in 1:nrow(model)){
    results[[i]] <- as.integer(model[i, 'actual'] == model[i, 'predicted'])
  }

  # calculates the number of correct predictions  
  correct <- sum(data.frame(results))
  
  # calculates the percent of correct predictions
  accuracy <- correct / nrow(model)

  return(accuracy)
}

### Train three models using k values of 4, 12, and 15
model_k4 <- train.model(4)
model_k12 <- train.model(12)
model_k15 <- train.model(15)

### Return accuracy for each model
k4_acc <- eval.model(model_k4)   # 0.7959184
k12_acc <- eval.model(model_k12) # 0.8571429
k15_acc <- eval.model(model_k15) # 0.8673469

### Selected model_k15 as the best performer among the three built
### Re-train model_k15 using the entire training dataset and score against the test dataset
model <- kknn(R1 ~., training_df, test_df, k = 15, scale = TRUE)

# Compare test_df R1 to predicted R1 values
results_df <- data.frame(test_df$R1, round(model$fitted.values))
names(results_df) <- c("actual","predicted")

# Calculate the accuracy of the model
model_accuracy <- eval.model(results_df) # 0.8163265