# Clear the environment
rm(list = ls())

# Comment in set.seed(33) to repeat results
set.seed(33) 

# Load crime data into a data frame
data_df <- read.table("germancredit.txt", header=FALSE)

# Change Response (V21) to 1 (good) or 0 (bad)
data_df$V21[data_df$V21 == 1] <- 1
data_df$V21[data_df$V21 == 2] <- 0

# Create training and test datasets
pct70 <- sample(1:nrow(data_df), size = round(0.7*(nrow(data_df))))
train_df <- data_df[pct70,]
test_df <- data_df[-pct70,]

# Build model with all factors to determine significant factors
model_all <- glm(V21 ~., family = binomial(link="logit"), train_df)
summary(model_all)

# Use step for variable selection
step_output <- step(model_all)
step_output

# Rebuild model with optimal step_output variable combination
model_refined <- glm(V21 ~ V1 + V2 + V3 + V4 + V5 + V6 + V7 + V8 + V9 + V10 + V14 + V15 + V20, family = binomial(link="logit"), data_df)
summary(model_refined)

# Predict using the model
model_refined_yhat <- predict(model_refined, test_df, type = "response")
yhat_pred <- as.integer(model_refined_yhat > 0.5)

# Create confusion matrix
table(yhat_pred, test_df$V21)

# Plot the ROC
require(pROC)
AUC <- roc(test_df$V21, yhat_pred)
plot(AUC, main = "ROC Curve")
AUC


###### PART 2

# Function to calculate the predicted cost
predCost <- function(threshold) {
  yhat_pred <- as.integer(model_refined_yhat > threshold)
  table <- as.matrix(table(yhat_pred, test_df$V21))
  cost <- table[2,1] + 5*table[1,2]
  return(cost)
}

# Create placeholder for results
results <- as.matrix(vector("list", 100))
i <- seq(8, 99, by=1)

# Loop through all values from 0.0 to 1.0
for (i in 8:99) {
  threshold <- (i/100)
  results[[i]] <- predCost(threshold)
}

# Display results
results

#Re-calculate accuracy and AUC for the min cost threshold
