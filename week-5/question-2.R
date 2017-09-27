# Clear the environment
rm(list = ls())

# Comment in set.seed(33) to repeat results
set.seed(33) 

# Load DAAG library for access to cv.lm
require(DAAG)

# Load crime data into a data frame
data_df <- read.table("uscrime.txt", header=TRUE)

# Model using all available factors
model_all <- lm(Crime ~., data_df)
summary(model_all)

# Create models using variety of factor configurations - only significant factors
model_1 <- lm(Crime ~ M + Ed + Po1 + U2 + Ineq + Prob, data_df) # all significant factors
model_2 <- lm(Crime ~ M + Ed + U2 + Ineq + Prob, data_df)       # all but Po1
model_3 <- lm(Crime ~ M + Ed + Po1 + Ineq + Prob, data_df)      # all but U2
model_4 <- lm(Crime ~ M + Ed + Ineq + Prob, data_df)            # * significance and above
model_5 <- lm(Crime ~ Ed + Ineq, data_df)                       # ** significance
    
# Assess each model using cross-validation and return Adj. R^2
cv_model_assessment <- function(model) {
  # Use cv.lm to perform CV on model
  cvmodel <- cv.lm(data_df, model, m = 5, printit = FALSE)
  
  # Sum of the Squared Errors
  ssres <- attr(cvmodel,"ms")*nrow(data_df)
  
  # Total Sum of Squared differences between data and its mean
  sstot <-  sum((data_df$Crime - mean(data_df$Crime))^2)
  
  # Calculate R^2
  r_sq <- 1 - (ssres/sstot)
  
  # Calculate Adj. R^2
  adj_r_sq_num <- (1-r_sq)*(nrow(data_df)) # numerator (1-R^2)*(n-1)
  adj_r_sq_den <- nrow(data_df)-(ncol(model$model)-1)-1 # denominator (n-k-1)
  adj_r_sq <- 1 - (adj_r_sq_num/adj_r_sq_den)
  
  return(adj_r_sq)  
}

# Find Adj. R^2 based on cv.lm for each model
ma_adj_r_sq <- cv_model_assessment(model_all)
m1_adj_r_sq <- cv_model_assessment(model_1)
m2_adj_r_sq <- cv_model_assessment(model_2)
m3_adj_r_sq <- cv_model_assessment(model_3)
m4_adj_r_sq <- cv_model_assessment(model_4)
m5_adj_r_sq <- cv_model_assessment(model_5)

# Display the Adj. R^2 values
ma_adj_r_sq # 0.111
m1_adj_r_sq # 0.575
m2_adj_r_sq # 0.053
m3_adj_r_sq # 0.553
m4_adj_r_sq # -0.11
m5_adj_r_sq # -0.0895

# Chose "best" model - model_1 
# model_1 uses all significant factors from model_all
summary(model_1)

# Data point to estimate for - note I'm only bringing in the variables model_1 requires
new_dp <- data.frame(M = 14.0, U2 = 3.6, Ineq = 20.1, Prob = 0.04, Po1 = 12.0, Ed = 10.0)
predict.lm(model_1, new_dp)
