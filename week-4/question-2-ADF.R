# Clear the enviroment
rm(list = ls())

require('forecast')

end_df <- read.table("end_dates.txt", header=TRUE)
end_ts <- ts(end_df[,2])

adf.test(end_ts)
