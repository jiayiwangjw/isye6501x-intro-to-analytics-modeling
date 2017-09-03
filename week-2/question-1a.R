# Clear the enviroment
rm(list = ls())

# Comment in set.seed(33) to repeat results across different values of k
set.seed(33) 

# Load the kknn library
require('kknn')

# Load credit card data into a data frame
data_df <- read.table("credit_card_data-headers.txt", header=TRUE)

# Randomly identify 80% of data points to use for creation of training_df
sample <- sample(nrow(data_df), round(nrow(data_df)*.8))

# Create training (80pct of records) and test (remaining 20pct of records) datasets
training_df <- data_df[sample, ]
test_df <- data_df[-sample, ]

### Create a function that trains a model using 10-fold cross-validation and returns the predictions
train.model <- function(neighbors){
  # train the model using 10-fold cv and the k value passed in as neighbors
  model <- cv.kknn(R1 ~., training_df, k = neighbors, kcv = 10, scale = TRUE)
  
  # store the predictions (yhat)
  model_fitted <- as.matrix(model[[1]])

  # round the yhat values
  model_rounded_yhat <- as.matrix(lapply(model_fitted[, 2], round))
  
  # merge rounded yhat values back into model_fitted
  model_fitted <- data.frame(model_fitted, model_rounded_yhat)
  
  # update column names
  colnames(model_fitted) <- c("y", "yhat", "yhat_rounded")
  
  # return the results
  return(model_fitted)
}

### Create a function that calculates the accuracy based on predictions
eval.model <- function(model){
  # instantiates a results vector of length nrow(model)
  results <- vector("list", nrow(model))  
  
  # loops through each record to check that y == predicted y
  for(i in 1:nrow(model)){
    results[[i]] <- as.integer(model[i, 'y'] == model[i, 'yhat_rounded'])
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
k4_acc <- eval.model(model_k4)   # 0.7992352
k12_acc <- eval.model(model_k12) # 0.8527725
k15_acc <- eval.model(model_k15) # 0.8661568

### Selected model_k15 as the best performer among the three built
### Re-train model_k15 using the entire training dataset and score against the test dataset
model <- kknn(R1 ~., training_df, test_df, k = 15, scale = TRUE)

# Compare test_df R1 to predicted R1 values
results_df <- data.frame(test_df$R1, round(model$fit))
names(results_df) <- c("y","yhat_rounded")

# Calculate the accuracy of the model
model_accuracy <- eval.model(results_df) # 0.8396947
