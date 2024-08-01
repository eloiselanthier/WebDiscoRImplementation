# Read data
data <- read.csv("Data_site_1.csv")

data$status <- 0

set.seed(13) # Setting seed for reproducibility (optional)
random_rows <- sample(1:nrow(data), 150)

# Assign 1 to the randomly selected rows in the status column
data$status[random_rows] <- 1

# Remove column
data <- subset(data, select = -c(id))

# Remove rows with NA
data <- na.omit(data)

# Move columns to the front
data <- data[, c("time", setdiff(names(data), "time"))]

# Write in CSV
write.csv(data, file="Data_site_1.csv", row.names=FALSE)
write.csv(data, file="Data_site_2.csv", row.names=FALSE)

