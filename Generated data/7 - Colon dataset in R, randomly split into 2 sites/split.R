# Read data
data <- read.csv("Data")

# Remove column
data <- subset(data, select = -c(id))

# Remove rows with NA
data <- na.omit(data)

# Move columns to the front
data <- data[, c("time", setdiff(names(data), "time"))]

# Write in CSV
write.csv(data[1:888,], file="Data_site_1.csv", row.names=FALSE)
write.csv(data[889:1776,], file="Data_site_2.csv", row.names=FALSE)

