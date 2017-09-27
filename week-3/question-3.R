# Clear the enviroment
rm(list = ls())

# Comment in set.seed(33) to repeat results across different values of k
set.seed(33) 

# Load the outliers library
require('qcc')

# Load the temp dataset
temps_df <- read.table("temps.txt", header=TRUE)

cusum(temps_df)
