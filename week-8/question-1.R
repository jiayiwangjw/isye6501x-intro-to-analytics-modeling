# Clear the environment
rm(list = ls())

# Comment in set.seed(33) to repeat results
set.seed(33) 

# Load glmnet and DAAG lib
require(glmnet)
require(DAAG)

# Load crime data into a data frame
data_df <- read.table("uscrime.txt", header=TRUE)

# Scale the data and convert it to a matrix for LASSO and ELNET
scaled_data_df <- as.data.frame(scale(data_df[,c(1,3:15)]))
scaled_data_df <- cbind(data_df[,2],scaled_data_df,data_df[,16])
colnames(scaled_data_df)[1] <- "So"
colnames(scaled_data_df)[16] <- "Crime"
data_mx <- as.matrix(scaled_data_df)

# Helper function to calculate R^2 - will be used later
ComputeR2 <- function(yhat_df, data_df) {
  SSres <- sum((yhat_df - data_df$Crime)^2)
  SStot <- sum((data_df$Crime - mean(data_df$Crime))^2)
  R2 <- 1 - SSres/SStot
  return(R2)
}

# Create a linear regression model with all factors
model_all <- lm(Crime ~., data_df)
step(model_all, direction = "both")

# Re-train model using "best" set of factors from step()
step_model <- lm(formula = Crime ~ M + Ed + Po1 + M.F + U1 + U2 + Ineq + Prob, data = data_df)
summary(step_model)

# Cross-validate the step_model
cv_step_model <- cv.lm(data = data_df, form.lm = step_model, m = 10)

# Calculate R^2 for the cv_step_model
step_yhat <- as.data.frame(cv_step_model$cvpred)
cv_step_model_R2 <- ComputeR2(step_yhat, data_df)
cv_step_model_R2 # 0.62



# Identify factors using LASSO
lasso_factors <- cv.glmnet(x = data_mx[,-16], 
                           y = data_mx[,"Crime"], 
                           alpha = 1, 
                           nfolds = 5,
                           type.measure = "mse",
                           family = "gaussian")

# Display the lambda.min for lasso_factors
lasso_factors$lambda.min # 4.82

# Display coefficients for lambda.min
lasso_coeff <- coef(lasso_factors, s = lasso_factors$lambda.min)
lasso_coeff

# Re-train model using lambda.min factors
lasso_model <- lm(formula = Crime ~ So + M + Ed + Po1 + M.F + Pop + NW + U1 + U2 + Wealth + Ineq + Prob, data = data_df)
summary(lasso_model)

# Cross-validate the lasso_model
cv_lasso_model <- cv.lm(data = data_df, form.lm = lasso_model, m = 10)
summary(cv_lasso_model)

# Calculate R^2 for the cv_step_model
lasso_yhat <- as.data.frame(cv_lasso_model$cvpred)
cv_lasso_model_R2 <- ComputeR2(lasso_yhat, data_df)
cv_lasso_model_R2 # 0.564



# Identify factors using Elastic Net and alpha of 0.25
elnet_factors <- cv.glmnet(x = data_mx[,-16], 
                           y = data_mx[,"Crime"], 
                           alpha = 0.25, 
                           nfolds = 5,
                           type.measure = "mse",
                           family = "gaussian")

# Display the lambda.min for elnet_factors
elnet_factors$lambda.min

# Display the coefficients for lamdba.min
elnet_coeff <- coef(elnet_factors, s = elnet_factors$lambda.min)
elnet_coeff

# Re-train model using lambda.min factors
elnet_model <- lm(formula = Crime ~ So + M + Ed + Po1 + Po2 + LF + M.F + Pop + NW + U1 + U2 + Wealth + Ineq + Prob, data = data_df)
summary(elnet_model)

# Cross-validate the elnet_model
cv_elnet_model <- cv.lm(data = data_df, form.lm = elnet_model, m = 10)
summary(cv_elnet_model)

# Calculate R^2 for the cv_elnet_model
elnet_yhat <- as.data.frame(cv_elnet_model$cvpred)
cv_elnet_model_R2 <- ComputeR2(elnet_yhat, data_df)
cv_elnet_model_R2 # 0.484


# Identify factors using Elastic Net and alpha of 0.50
elnet_factors <- cv.glmnet(x = data_mx[,-16], 
                           y = data_mx[,"Crime"], 
                           alpha = 0.50, 
                           nfolds = 5,
                           type.measure = "mse",
                           family = "gaussian")

# Display the lambda.min for elnet_factors
elnet_factors$lambda.min

# Display the coefficients for lamdba.min
elnet_coeff <- coef(elnet_factors, s = elnet_factors$lambda.min)
elnet_coeff

# Re-train model using lambda.min factors
elnet_model <- lm(formula = Crime ~ So + M + Ed + Po1 + Po2 + M.F + Pop + NW + U1 + U2 + Wealth + Ineq + Prob, data = data_df)
summary(elnet_model)

# Cross-validate the elnet_model
cv_elnet_model <- cv.lm(data = data_df, form.lm = elnet_model, m = 10)
summary(cv_elnet_model)

# Calculate R^2 for the cv_elnet_model
elnet_yhat <- as.data.frame(cv_elnet_model$cvpred)
cv_elnet_model_R2 <- ComputeR2(elnet_yhat, data_df)
cv_elnet_model_R2 # 0.529


# Identify factors using Elastic Net and alpha of 0.75
elnet_factors <- cv.glmnet(x = data_mx[,-16], 
                           y = data_mx[,"Crime"], 
                           alpha = 0.75, 
                           nfolds = 5,
                           type.measure = "mse",
                           family = "gaussian")

# Display the lambda.min for elnet_factors
elnet_factors$lambda.min

# Display the coefficients for lamdba.min
elnet_coeff <- coef(elnet_factors, s = elnet_factors$lambda.min)
elnet_coeff

# Re-train model using lambda.min factors
elnet_model <- lm(formula = Crime ~ So + M + Ed + Po1 + M.F + Pop + NW + U1 + U2 + Wealth + Ineq + Prob, data = data_df)
summary(elnet_model)

# Cross-validate the elnet_model
cv_elnet_model <- cv.lm(data = data_df, form.lm = elnet_model, m = 10)
summary(cv_elnet_model)

# Calculate R^2 for the cv_elnet_model
elnet_yhat <- as.data.frame(cv_elnet_model$cvpred)
cv_elnet_model_R2 <- ComputeR2(elnet_yhat, data_df)
cv_elnet_model_R2 # 0.564

