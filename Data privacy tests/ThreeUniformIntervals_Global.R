# Includes
library("survival")
library("survminer")
library("dplyr")
library(ggplot2)

data1 <- read.csv("Data_site_1.csv")
data2 <- read.csv("Data_site_2.csv")
data3 <- read.csv("Data_site_3.csv")

# Graphique
combined_data <- rbind(transform(data1, dataset = "Dataset 1"),
                       transform(data2, dataset = "Dataset 2"),
                       transform(data3, dataset = "Dataset 3"))

# Create density plots for each dataset
combined_plot <- ggplot(combined_data, aes(x = time, color = dataset)) +
  geom_density() +
  labs(title = "Density Plot of Time", x = "Time", y = "Density") +
  theme_minimal()

# Display the combined plot
print(combined_plot)

# Standard
dataOG <- rbind(data1, data2, data3)

# Constant intervals - before cutoff
lowcutoff <- 150
cutoff <- 580

data1 <- data1[data1$time <= lowcutoff, ]
data1 <- data1[order(data1$time), ]
array1 <- data1$time

differences1 <- abs(array1 - c(array1[c(5:length(array1), 1, 2)], array1[(length(array1) - 1):length(array1)]))
max_difference1 <- max(differences1[1:(length(differences1) - 4)])
min_array_1 <- min(array1)
max_array_1 <- max(array1)

data2 <- data2[data2$time <= lowcutoff, ]
data2 <- data2[order(data2$time), ]
array2 <- data2$time
differences2 <- abs(array2 - c(array2[c(5:length(array2), 1, 2)], array2[(length(array2) - 1):length(array2)]))
max_difference2 <- max(differences2[1:(length(differences2) - 4)])
min_array_2 <- min(array2)
max_array_2 <- max(array2)

data3 <- data3[data3$time <= lowcutoff, ]
data3 <- data3[order(data3$time), ]
array3 <- data3$time
differences3 <- abs(array3 - c(array3[c(5:length(array3), 1, 2)], array3[(length(array3) - 1):length(array3)]))
max_difference3 <- max(differences3[1:(length(differences3) - 4)])
min_array_3 <- min(array3)
max_array_3 <- max(array3)

max_difference <- max(max_difference1, max_difference2, max_difference3)
max_difference

min_total <- min(array1, array2, array3)
max_total <- max(array1, array2, array3)
min_total
max_total

intervals <- seq(from = min_total, to = max_total, by = max_difference)
if (max_total - intervals[length(intervals)] < max_difference) {
  intervals[length(intervals)] <- max_total
}
intervals

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

data1_first <- modify_time(data1, intervals)
data2_first <- modify_time(data2, intervals)
data3_first <- modify_time(data3, intervals)

# Milieu
data1 <- read.csv("Data_site_1.csv")
data2 <- read.csv("Data_site_2.csv")
data3 <- read.csv("Data_site_3.csv")

data1 <- data1[data1$time > lowcutoff & data1$time <= cutoff, ]
data1 <- data1[order(data1$time), ]
array1 <- data1$time

differences1 <- abs(array1 - c(array1[c(5:length(array1), 1, 2)], array1[(length(array1) - 1):length(array1)]))
max_difference1 <- max(differences1[1:(length(differences1) - 4)])
min_array_1 <- min(array1)
max_array_1 <- max(array1)

data2 <- data2[data2$time > lowcutoff & data2$time <= cutoff, ]
data2 <- data2[order(data2$time), ]
array2 <- data2$time
differences2 <- abs(array2 - c(array2[c(5:length(array2), 1, 2)], array2[(length(array2) - 1):length(array2)]))
max_difference2 <- max(differences2[1:(length(differences2) - 4)])
min_array_2 <- min(array2)
max_array_2 <- max(array2)

data3 <- data3[data3$time > lowcutoff & data3$time <= cutoff, ]
data3 <- data3[order(data3$time), ]
array3 <- data3$time
differences3 <- abs(array3 - c(array3[c(5:length(array3), 1, 2)], array3[(length(array3) - 1):length(array3)]))
max_difference3 <- max(differences3[1:(length(differences3) - 4)])
min_array_3 <- min(array3)
max_array_3 <- max(array3)

max_difference <- max(max_difference1, max_difference2, max_difference3)
max_difference

min_total <- min(array1, array2, array3)
max_total <- max(array1, array2, array3)
min_total
max_total

intervals <- seq(from = min_total, to = max_total, by = max_difference)
if (max_total - intervals[length(intervals)] < max_difference) {
  intervals[length(intervals)] <- max_total
}
intervals

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

data1_before <- modify_time(data1, intervals)
data2_before <- modify_time(data2, intervals)
data3_before <- modify_time(data3, intervals)

# Intervals after cutoff
data1 <- read.csv("Data_site_1.csv")
data2 <- read.csv("Data_site_2.csv")
data3 <- read.csv("Data_site_3.csv")

data1 <- data1[data1$time > cutoff, ]
data1 <- data1[order(data1$time), ]
array1 <- data1$time

differences1 <- abs(array1 - c(array1[c(5:length(array1), 1, 2)], array1[(length(array1) - 1):length(array1)]))
max_difference1 <- max(differences1[1:(length(differences1) - 4)])
min_array_1 <- min(array1)
max_array_1 <- max(array1)

data2 <- data2[data2$time > cutoff, ]
data2 <- data2[order(data2$time), ]
array2 <- data2$time

differences2 <- abs(array2 - c(array2[c(5:length(array2), 1, 2)], array2[(length(array2) - 1):length(array2)]))
max_difference2 <- max(differences2[1:(length(differences2) - 4)])
min_array_2 <- min(array2)
max_array_2 <- max(array2)

data3 <- data3[data3$time > cutoff, ]
data3 <- data3[order(data3$time), ]
array3 <- data3$time

differences3 <- abs(array3 - c(array3[c(5:length(array3), 1, 2)], array3[(length(array3) - 1):length(array3)]))
max_difference3 <- max(differences3[1:(length(differences3) - 4)])
min_array_3 <- min(array3)
max_array_3 <- max(array3)

max_difference <- max(max_difference1, max_difference2, max_difference3)
max_difference

min_total <- min(array1, array2, array3)
max_total <- max(array1, array2, array3)
min_total
max_total

intervals <- seq(from = min_total, to = max_total, by = max_difference)
if (max_total - intervals[length(intervals)] < max_difference) {
  intervals[length(intervals)] <- max_total
}
intervals

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

data1_after <- modify_time(data1, intervals)
data2_after <- modify_time(data2, intervals)
data3_after <- modify_time(data3, intervals)

# Merge both interval types
data1 <- rbind(data1_first, data1_before, data1_after)
data2 <- rbind(data2_first, data2_before, data2_after)
data3 <- rbind(data3_first, data3_before, data3_after)

dataInt <- rbind(data1, data2, data3)
dataInt <- dataInt[order(dataInt$time), ]

# Original data
res.cox.OG <- coxph(Surv(time, status) ~ age + sex + ph.ecog + ph.karno + pat.karno + meal.cal + wt.loss, dataOG)
#res.cox.OG <- coxph(Surv(time, status) ~ X1 + X2 + X3 + X4 + X5 + X6, dataOG)
res.cox.OG
summary(res.cox.OG)


# Data intervals
res.cox.Int <- coxph(Surv(time, status) ~ age + sex + ph.ecog + ph.karno + pat.karno + meal.cal + wt.loss, dataInt)
#res.cox.Int <- coxph(Surv(time, status) ~ X1 + X2 + X3 + X4 + X5 + X6, dataInt)
res.cox.Int
summary(res.cox.Int)

# Sum de l'erreur relative
coefficients.OG <- coef(res.cox.OG)
coefficients.Int <- coef(res.cox.Int)
differences <- coefficients.OG - coefficients.Int
percentage_errors <- (differences / coefficients.OG) * 100
absolute_errors <- abs(percentage_errors)
error <- sum(absolute_errors)
error






# Create histogram for the 'time' column in dataInt
plot_dataInt_histogram <- ggplot(dataInt, aes(x = time)) +
  geom_histogram(binwidth = 1, fill = "blue", color = "black", alpha = 0.7) +
  labs(title = "Histogram of Time", x = "Time", y = "Count") +
  theme_minimal()

# Display the plot
print(plot_dataInt_histogram)