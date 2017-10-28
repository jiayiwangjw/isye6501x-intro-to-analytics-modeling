# Clear the environment
rm(list = ls())

# Comment in set.seed(33) to repeat results
set.seed(33) 

# Load tree lib
require(tree)
require(randomForest)

# Load crime data into a data frame
data_df <- read.table("uscrime.txt", header=TRUE)

# Train tree model
tree_model <- tree(Crime ~., data_df)
summary(tree_model)

# Visualize tree model
plot(tree_model)
text(tree_model)

# Function to calculate R^2
ComputeR2 <- function(yhat_df, data_df) {
  SSres <- sum((yhat_df - data_df$Crime)^2)
  SStot <- sum((data_df$Crime - mean(data_df$Crime))^2)
  R2 <- 1 - SSres/SStot
  return(R2)
}

# Calculate R^2
tree_yhat <- predict(tree_model)
tree_r2 <- ComputeR2(tree_yhat, data_df)
tree_r2

# Manually prune tree
tree_model_pruned <- prune.tree(tree_model,best = 4)
summary(tree_model_pruned)

# Visualize pruned tree
plot(tree_model_pruned)
text(tree_model_pruned)

# Calc R^2
pruned_yhat <- predict(tree_model_pruned)
pruned_r2 <- ComputeR2(pruned_yhat, data_df)
pruned_r2

##################################################

# Use 1+log(n) standard to pick number of factors in each set
factor_set <- round(1 + log(ncol(data_df)))

# Train forest model
forest_model <- randomForest(Crime ~., data_df, mtry = factor_set, importance = TRUE, ntree = 500)
forest_model

# Use R2 function to calculate
forest_model_yhat <- predict(forest_model)
ComputeR2(forest_model_yhat, data_df)

# Visualize variable importance
importance(forest_model)
varImpPlot(forest_model)


predict(tree_model_pruned, data_df[,1:15])
predict(forest_model, data_df[,1:15])
