#Load the kknn library
require('kknn')

# Load credit card data into a data frame
data_df <- read.table("credit_card_data-headers.txt", header=TRUE)

# Identifies 80% of the records that will be used to determine the training_df
sample <- sample(nrow(data_df), round(nrow(data_df)*.8))

# Create training (80pct) and testing (20pct) samples
training_df <- data_df[sample, ]
testing_df <- data_df[-sample, ]

#model <- kknn(formula = formula(data_df), data_df, data_df, k = 3, distance = 2, kernel = "optimal", scale = TRUE)

#results <- predict(model, data_mx[,1:10])


Data <- iris
head(Data, 10)
Sample <- sample(1:150, 50)
testing <- Data[Sample, ]
learning <- Data[-Sample, ]
dim(Data)
dim(learning)
dim(testing)
suppressWarnings(suppressMessages(library(kknn)))
model <- train.kknn(Species ~ ., data = learning, kmax = 9)
model

prediction <- predict(model, testing[, -5])
prediction
CM <- table(testing[, 5], prediction)
CM
accuracy <- (sum(diag(CM)))/sum(CM)
accuracy
plot(model)