#Load the kernlab library
require(kernlab)

# Load credit card data and convert to matrix
data_df <- read.table("credit_card_data-headers.txt", header=TRUE)
data_mx <- data.matrix(data_df)

# Train the model
model <- ksvm(
  data_mx[,1:10],         # predictors
  data_mx[,11],           # response
  kernel="vanilladot",    # kernel selector
  type="C-svc",           # ksvm type
  C=100,                  # cost of constraints violation
  scaled = TRUE           # scale flag
  )

# Print model error
model@error

# Score data_mx against the model
results <- predict(model, data_mx[,1:10], type='response')

# Calculate number of predicted Positive applications (actual is 354)
sum(results)

# calculate a1…am for model
a <- colSums(model@xmatrix[[1]] * model@coef[[1]])
a

# calculate a0
a0 <- -model@b
a0



