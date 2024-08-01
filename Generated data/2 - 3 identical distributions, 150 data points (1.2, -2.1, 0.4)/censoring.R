
path <- dirname(rstudioapi::getActiveDocumentContext()$path)
setwd(path)

k <- 1
percent_censored <- 50
set.seed(358)

# Read data
data <- read.csv(paste0("Data_site_", k, ".csv"))
data$status <- 0

nbCensored <- nrow(data) * percent_censored/100
random_rows <- sample(1:nrow(data), nbCensored)

# Assign 1 to the randomly selected rows in the status column
data$status[random_rows] <- 1

# Write in CSV
write.csv(data, file=paste0("Data_site_", k, ".csv"), row.names=FALSE)

