# Clear the enviroment
rm(list = ls())

# Comment in set.seed(33) to repeat results across different values of k
set.seed(33) 

# Load the outliers library
require('outliers')

# Load crime data into a data frame
uscrime_df <- read.table("uscrime.txt", header=TRUE)

# Use grubbs.test to check if lowest and highest value are two outliers on opposite tails of sample
grubbs.test(uscrime_df$Crime, type = 11, opposite = FALSE, two.sided = TRUE)

# Use grubbs.test to check if highest value is an outlier
grubbs.test(uscrime_df$Crime, type = 10, opposite = FALSE, two.sided = FALSE)

# Use grubbs.test to check if lowest value is an outlier
grubbs.test(uscrime_df$Crime, type = 10, opposite = TRUE, two.sided = FALSE)

# Plot usdata_df$crime as a boxplot
boxplot(uscrime_df$Crime,data=uscrime_df$Crime, main="US Crime Rate per 100k")

temps_df <- read.table("temps.txt", header=TRUE)
write.csv(temps_df)
