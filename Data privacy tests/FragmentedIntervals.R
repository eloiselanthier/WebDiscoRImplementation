# Includes
library("survival")
library("survminer")
library("dplyr")

# Read files
data1 <- read.csv("Data_site_1.csv")
data2 <- read.csv("Data_site_2.csv")
data3 <- read.csv("Data_site_3.csv")

dataOG <- rbind(data1, data2, data3)

# Define number of data points min in each interval
nbDataPts <- 5

# Find min and max values
data1 <- data1[order(data1$time), ]
data2 <- data2[order(data2$time), ]
data3 <- data3[order(data3$time), ]

min_total <- min(min(data1$time), min(data2$time), min(data3$time))
max_total <- max(max(data1$time), max(data2$time), max(data3$time))


# Define the split_interval function
split_interval <- function(interval, condition_fn) {
  min_val <- interval[1]
  max_val <- interval[2]
  
  # Calculate the midpoint
  mid_val <- (min_val + max_val) / 2
  
  # Create temporary sub-intervals
  left_interval_temp <- c(min_val, mid_val)
  right_interval_temp <- c(mid_val, max_val)
  
  # Check the condition for the temporary sub-intervals
  if (condition_fn(min_val, mid_val) && condition_fn(mid_val, max_val)) {
    # Recursively split the sub-intervals if they satisfy the condition
    left_split <- split_interval(left_interval_temp, condition_fn)
    right_split <- split_interval(right_interval_temp, condition_fn)
    
    # Combine the results
    return(c(left_split, right_split))
  } else {
    # If the condition is not met for either sub-interval, return the original interval
    return(list(interval))
  }
}

# Define the example condition function
example_condition <- function(data, min_val, max_val, nbDataPts) {
  count <- sum(data$time >= min_val & data$time < max_val)
  return(count >= nbDataPts)
}

# Function to process multiple intervals and datasets
process_intervals <- function(interval, data, nbDataPts) {
  condition_fn <- function(min_val, max_val) {
    example_condition(data, min_val, max_val, nbDataPts)
  }
  split_interval(interval, condition_fn)
}

# Example usage
intervals1 <- c(min_total,max_total)
intervals2 <- c(min_total,max_total)
intervals3 <- c(min_total,max_total)

# Assume data1, data2, and data3 are your data frames
result1 <- process_intervals(intervals1, data1, nbDataPts)
result2 <- process_intervals(intervals2, data2, nbDataPts)
result3 <- process_intervals(intervals3, data3, nbDataPts)

# Function to extract unique values from the result
extract_unique_values <- function(result) {
  unique_values <- unique(unlist(result))
  sort(unique_values)
}

# Get unique values from each result
unique_values1 <- extract_unique_values(result1)
unique_values2 <- extract_unique_values(result2)
unique_values3 <- extract_unique_values(result3)

print(unique_values1)
print(unique_values2)
print(unique_values3)

intervals <- Reduce(intersect, list(unique_values1, unique_values2, unique_values3))

interval_averages <- sapply(1:(length(intervals) - 1), function(i) {
  mean(c(intervals[i], intervals[i + 1]))
})


modify_time <- function(data, intervals) {
  interval_averages <- sapply(1:(length(intervals) - 1), function(i) {
    mean(c(intervals[i], intervals[i + 1]))
  })
  data$time <- sapply(data$time, function(time_value) {
    for (i in 1:(length(intervals) - 1)) {
      if (time_value >= intervals[i] && time_value < intervals[i + 1]) {
        return(interval_averages[i])
      }
    }
    return(interval_averages[length(interval_averages)])
  })
  return(data)
}

data1 <- modify_time(data1, intervals)
data2 <- modify_time(data2, intervals)
data3 <- modify_time(data3, intervals)

# Données finales
dataInt <- rbind(data1, data2, data3)
dataInt <- dataInt[order(dataInt$time), ]

# Error
#res.cox.OG <- coxph(Surv(time, status) ~ age + sex + ph.ecog + ph.karno + pat.karno + meal.cal + wt.loss, dataOG)
#res.cox.Int <- coxph(Surv(time, status) ~ age + sex + ph.ecog + ph.karno + pat.karno + meal.cal + wt.loss, dataInt)

res.cox.OG <- coxph(Surv(time, status) ~ X1 + X2 + X3, dataOG)
res.cox.Int <- coxph(Surv(time, status) ~ X1 + X2 + X3, dataInt)

#res.cox.OG <- coxph(Surv(time, status) ~ X1 + X2 + X3 + X4 + X5 + X6, dataOG)
#res.cox.Int <- coxph(Surv(time, status) ~ X1 + X2 + X3 + X4 + X5 + X6, dataInt)

differences <- coef(res.cox.OG) - coef(res.cox.Int)
error <- sum(abs((differences / coef(res.cox.OG)) * 100))
error


# Create histogram for the 'time' column in dataInt
plot_dataInt_histogram <- ggplot(dataInt, aes(x = time)) +
  geom_histogram(binwidth = 1, fill = "blue", color = "black", alpha = 0.7) +
  labs(title = "Histogram of Time", x = "Time", y = "Count") +
  theme_minimal()

# Display the plot
print(plot_dataInt_histogram)
