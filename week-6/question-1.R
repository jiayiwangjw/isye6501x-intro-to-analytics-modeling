# Clear the environment
rm(list = ls())

# Comment in set.seed(33) to repeat results
set.seed(33) 

# Load crime data into a data frame
data_df <- read.table("uscrime.txt", header=TRUE)

# Perform PCA on data_df
pca <- prcomp(data_df[,1:15], scale = TRUE)
summary(pca)

# Visualize PCA results
plot(pca)
biplot(pca)

# Extract the 1st four components and append Crime
pca_df <- data.frame(cbind(pca$x[,1:4],data_df$Crime))
names(pca_df) <- c('PC1','PC2','PC3','PC4','Crime')

# Create lm model using pca dataset
model_pca <- lm(Crime ~., pca_df)

# Display summary
summary(model_pca)

# Convert model_pca coefficients to original factors
coefficients_converted <- (pca$rotation[,1:4] %*% model_pca$coefficients[2:5])/pca$scale

# Adjust intercept based on pca$center
intercept <- model_pca$coefficients[1] - sum(coefficients_converted * pca$center)

# New data point that we'll predict Crime for
new_dp <- data.frame(M = 14.0, So = 0, Ed = 10.0, Po1 = 12.0, Po2 = 15.5, 
                     LF = 0.640, M.F = 94.0, Pop = 150, NW = 1.1, U1 = 0.120, 
                     U2 = 3.6, Wealth = 3200, Ineq = 20.1, Prob = 0.04, Time = 39.0)

# Manually calculate Crime for new_dp using coefficients_coverted and intercept
### Would like to refactor into more elegant method
Crime <- sum(
  coefficients_converted[1,1] %*% new_dp$M,
  coefficients_converted[2,1] %*% new_dp$So,
  coefficients_converted[3,1] %*% new_dp$Ed,
  coefficients_converted[4,1] %*% new_dp$Po1,
  coefficients_converted[5,1] %*% new_dp$Po2,
  coefficients_converted[6,1] %*% new_dp$LF,
  coefficients_converted[7,1] %*% new_dp$M.F,
  coefficients_converted[8,1] %*% new_dp$Pop,
  coefficients_converted[9,1] %*% new_dp$NW,
  coefficients_converted[10,1] %*% new_dp$U1,
  coefficients_converted[11,1] %*% new_dp$U2,
  coefficients_converted[12,1] %*% new_dp$Wealth,
  coefficients_converted[13,1] %*% new_dp$Ineq,
  coefficients_converted[14,1] %*% new_dp$Prob,
  coefficients_converted[15,1] %*% new_dp$Time,
  intercept
  )

Crime # 1112.678
