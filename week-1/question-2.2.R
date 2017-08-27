#Load the kknn library
require('kknn')

# Load credit card data into a data frame
data_df <- read.table("credit_card_data-headers.txt", header=TRUE)

# set.seed(42) - comment in to repeat results across different values of k

# Identifies 70% of the records that will be used to determine the train_df
sample <- sample(nrow(data_df), round(nrow(data_df)*.7))

# Create training (80pct) and test (20pct) datasets
train_df <- data_df[sample, ]
test_df <- data_df[-sample, ]

# Train the model - test using full dataset per homework instructions
model <- kknn(R1 ~ ., train_df, data_df, k = 4, scale = TRUE)

# Compare data_df R1 to model fit
results_df <- data.frame(data_df$R1, model$fit)

# Create a confusion matrix - round model$fit since fit is continuous and R1 is 0 or 1
CM <- table(data_df[, 11], round(model$fit))

# Calculate accuracy using CM
accuracy <- (sum(diag(CM)))/sum(CM)

# Print accuracy
accuracy



