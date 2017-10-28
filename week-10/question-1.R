# Clear the environment
rm(list = ls())

# Comment in set.seed(33) to repeat results
set.seed(33) 

# Load cancer data into a data frame
data_df <- read.table("breast-cancer-wisconsin.data.txt", header=FALSE, sep=",", stringsAsFactors = TRUE)

# Rename data_df headers
colnames(data_df) <- c("id", 
                       "clump_thickness", 
                       "unif_cell_size", 
                       "unif_cell_shape", 
                       "marg_adhesion", 
                       "single_epi_cell_size", 
                       "bare_nuclei", 
                       "bland_chromatin", 
                       "norm_nucleoli", 
                       "mitoses", 
                       "class")

# Update class field from 2/4 to 0/1
data_df$class[data_df$class == 2] <- '0'
data_df$class[data_df$class == 4] <- '1'

# Replace ? with NA in data_df
data_df[data_df=='?'] <- ''

# Identify fields with missing data
summary(data_df)

# Calculate the mode
# Source: https://stackoverflow.com/users/169947/ken-williams
Mode <- function(x) {
  ux <- unique(x)
  ux[which.max(tabulate(match(x, ux)))]
}

# Impute nulls with mode (due to ordinal scale of bare_nuclei)
data_df$bare_nuclei[is.na(data_df[,'bare_nuclei'])] <- Mode(data_df[,'bare_nuclei'])
summary(data_df)
