# Clear the enviroment
rm(list = ls())

# Comment in set.seed(33) to repeat results across different values of k
set.seed(33) 

# Create function to apply kmeans using parameterized nstart and algorithm values
iris.cluster <- function(start, algo) {
  # Apply kmeans to the iris_df; setting nstart to start and algorithm to algo
  iris_clusters <- kmeans(iris[, -5], 3, iter.max = 100000, nstart = start, algorithm = algo)
  
  # Append fit to the iris_df
  iris_df <- cbind(iris, fitted(iris_clusters, method = "classes"))
  
  # Update the column name from fitted df name to 'Cluster'
  colnames(iris_df)[6] <- 'Cluster'
  
  # Assign species names in place of cluster numbers using rename.cluster function
  iris_df <- rename.cluster(iris_df)
  
  # Create confusion matrix
  CM <- table(iris_df$Species, iris_df$Cluster)
  
  # Calculate accuracy using CM
  accuracy <- (sum(diag(CM)))/sum(CM)
  
  return(accuracy)
}

# Create function to rename the cluster column based on dominant species
rename.cluster <- function(df) {
  # Function to get the mode of the input
  getmode <- function(v) {
    uniqv <- unique(v)
    uniqv[which.max(tabulate(match(v, uniqv)))]
  }
  
  # Subset iris dataset based on species
  setosa_df <- df[ which(df$Species == 'setosa'), ]
  versicolor_df <- df[ which(df$Species == 'versicolor'), ]
  virginica_df <- df[ which(df$Species == 'virginica'), ]
  
  # Calculate mode of cluster column for each species
  setosa_mode <- getmode(setosa_df$Cluster)
  virginica_mode <- getmode(virginica_df$Cluster)
  versicolor_mode <- getmode(versicolor_df$Cluster)
  
  # Assign a Species value to the cluster column using species mode
  df$Cluster[df$Cluster == setosa_mode] <-'setosa'
  df$Cluster[df$Cluster == virginica_mode] <-'virginica'
  df$Cluster[df$Cluster == versicolor_mode] <-'versicolor'
  
  return(df)
}

# Iterate through iris.cluster 100x using the specified algorithm; produces a vector of accuracies based on algorithm and nstart
algo.cluster <- function(algo){
  # instantiates a results vector of length 100
  results <- vector("list", 100)  
  
  # loops iris.cluster for 1:100 values of nstart
  for(i in 1:100){
    results[[i]] <- iris.cluster(i, algo)
  }
  
  return(results)
}

# Apply algo.cluster to each algorithm
hartigan <- algo.cluster("Hartigan-Wong")
lloyd <- algo.cluster("Lloyd")
macqueen <- algo.cluster("MacQueen")

# Combine the results
results <- cbind(hartigan, lloyd, macqueen)

### Create scatter plot matrices for data investigation
# Species as dot color
# pairs(~Petal.Width+Petal.Length+Sepal.Length+Sepal.Width, data = iris_df, main = 'Iris Scatterplot Matrix', col = iris_df$Species)
# Cluster as dot color
# pairs(~Petal.Width+Petal.Length+Sepal.Length+Sepal.Width, data = iris_df, main = 'Iris Scatterplot Matrix', col = iris_df$Cluster)