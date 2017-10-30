# Clear the environment
rm(list = ls())

# Comment in set.seed(33) to repeat results
set.seed(33) 

# Load dplyr lib
require(dplyr)

# Create function to re-load data, since we'll want to start with a fresh dataset for each part
load_data <- function() {
  # Load cancer data into a data frame
  data_df <- read.table("breast-cancer-wisconsin.data.txt", header=FALSE, sep=",", stringsAsFactors = TRUE)

  # Update V11 (response) field from 2/4 to 0/1
  data_df$V11[data_df$V11 == 2] <- 0
  data_df$V11[data_df$V11 == 4] <- 1

  # Replace ? with NA in data_df
  data_df[data_df=='?'] <- ''
  
  # Return data_df
  return(data_df)
}



### PART 0: Identifiy fields with missing data
# Load data into a data frame
data_df <- load_data()

# Change data_df into dplyr table
data_tbl <- tbl_df(data_df)

# Function to identify columns with missing data
Find_Columns_with_Missing <- function(table, column) {
  filtered_tbl <- filter(table, is.na(table[column]))
  records <- nrow(filtered_tbl)
  
  return(records)
}

# Create placeholder for Find_Columns_with_Missing results
missing_tbl <- tbl_df(colnames(data_tbl))

# Loop through each column in data_tbl
for (i in 1:nrow(missing_tbl)) {
  missing_tbl[i,2] <- Find_Columns_with_Missing(data_tbl, i)
}

# Filter to only show columns with missing variables
cols_w_na_data <- filter(missing_tbl, missing_tbl[2]>0)
# V7 has 16 missing values
cols_w_na_data



### PART 1: Impute using mode
# Load data into a data frame
data_df <- load_data()
# Function to calculate the mode
# Source: https://stackoverflow.com/users/169947/ken-williams
Mode <- function(x) {
  ux <- unique(x)
  ux[which.max(tabulate(match(x, ux)))]
}

# Impute nulls with mode (due to ordinal scale of bare_nuclei)
data_df$V7[is.na(data_df[,'V7'])] <- Mode(data_df[,'V7'])
data_df <- transform(data_df, V7 = as.numeric(as.character(V7)))
summary(data_df)




### PART 2: Impute using Regression
# Load data into a data frame
data_df <- load_data()

# Splice table into records with/without missing data
data_df_w_na <- filter(data_df, is.na(data_df$V7))
data_df_wo_na <- filter(data_df, !is.na(data_df$V7))

# Create a linear regression model
imputation_model <- lm(as.numeric(V7) ~ V2 + V3 + V4 + V5 + V6 + V8 + V9 + V10, data_df_wo_na)
summary(imputation_model)

# Use stepwise for factor selection
step(imputation_model, direction = "backward")

# Re-train the linear regression using stepwise recommended factors
step_model <- lm(as.numeric(V7) ~ V2 + V3 + V4 + V5 + V9, data_df_wo_na)
summary(step_model)

# Predict values for V7 and round to convert to integers
V7 <- data.frame(round(predict(step_model, data_df_w_na)))
colnames(V7) <- c("V7")

# Impute the predictions to data_df_w_na
data_df_w_na <- cbind(data_df_w_na[,1:6], V7, data_df_w_na[,8:11])

# Combine data_df_w_na and data_df_wo_na into imputed_data_df
imputed_data_df <- rbind(data_df_w_na[,1:11], data_df_wo_na[,1:11])
imputed_data_df <- transform(imputed_data_df, V7 = as.numeric(V7))
summary(imputed_data_df)



### PART 3: Impute using Regression with Perturbation
# Load data into a data frame
data_df <- load_data()

# Splice table into records with/without missing data
data_df_w_na <- filter(data_df, is.na(data_df$V7))
data_df_wo_na <- filter(data_df, !is.na(data_df$V7))

# Create a linear regression model
imputation_model <- lm(as.numeric(V7) ~ V2 + V3 + V4 + V5 + V6 + V8 + V9 + V10, data_df_wo_na)
summary(imputation_model)

# Use stepwise for factor selection
step(imputation_model, direction = "backward")

# Re-train the linear regression using stepwise recommended factors
step_model <- lm(as.numeric(V7) ~ V2 + V3 + V4 + V5 + V9, data_df_wo_na)

# CV the step_model
cv_step_model <- cv.lm(data_df_wo_na, step_model)

# Predict values for V7
V7 <- data.frame(predict(step_model, data_df_w_na))

# Create a normal distribution for perturbation
normal_dist <- data.frame(rnorm(nrow(V7), mean = 0, sd = 1))

# Add perturbation to predicted V7 values and round
perturbed_V7 <- data.frame(round(V7[,1] + normal_dist[,1]))
colnames(perturbed_V7) <- c("V7")

# Impute the predictions to data_df_w_na
data_df_w_na <- cbind(data_df_w_na[,1:6], perturbed_V7, data_df_w_na[,8:11])

# Combine data_df_w_na and data_df_wo_na into imputed_data_df
imputed_data_df <- rbind(data_df_w_na[,1:11], data_df_wo_na[,1:11])
imputed_data_df <- transform(imputed_data_df, V7 = as.numeric(as.character(V7)))
summary(imputed_data_df)

# Update min value of V7 to fit 1:10 scale
imputed_data_df$V7[imputed_data_df$V7 == 0] <- 1
summary(imputed_data_df)
