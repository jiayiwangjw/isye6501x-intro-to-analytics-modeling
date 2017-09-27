# Clear the enviroment
rm(list = ls())

# Comment in set.seed(33) to repeat results
set.seed(33) 

# Read in temps.txt into a data frame
temps_df <- read.table("temps.txt", header=TRUE)

# Add an overall average column to temps_df
avg.temps <- function(x) {
  return(mean(as.numeric(temps_df[x,2:21])))
}
Overall_Avg <- matrix(0, ncol = 1, nrow = 123)  
for(i in 1:123) {
  Overall_Avg[i,1] <- avg.temps(i)
}
temps_df <- cbind(temps_df, Overall_Avg)

# Convert temps_df to a time-series object
temps_ts <- ts(temps_df[,2:22])

# Function that creates an es model for each year and returns fitted values
es.model <- function(year) {
  
  # Passes in data for each year and set to only use trending
  model <- HoltWinters(temps_ts[,year], gamma = FALSE)
  
  # Return the fitted temp values for the model
  return(data.frame(model$fitted[,'xhat']))

}

# Create the data frame structure for fitted temps
temps_fitted <- data.frame(temps_df[3:123, 'DAY'])  

# Loop through each year of temp data
for(i in 1:21) {
  temps_fitted <- cbind(temps_fitted, es.model(i))
}

# Update column names to match temps_df
colnames(temps_fitted) <- names(temps_df)

# Output temps_fitted for CUSUM calculation in Alteryx and visualization in Tableau
write.csv(temps_fitted, file = "temps_fitted.csv")
