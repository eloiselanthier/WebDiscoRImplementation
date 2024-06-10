# Includes
library("survival")
library("survminer")
library("dplyr")

data1 <- read.csv("Data_site_1.csv")
data2 <- read.csv("Data_site_2.csv")
data3 <- read.csv("Data_site_3.csv")

# Standard
dataOG <- rbind(data1, data2, data3)

# En faisant la moyenne des cinq données de temps

data1 <- data1[order(data1$time), ]
data1 <- data1 %>%
  mutate(group = (row_number() - 1) %/% 5 + 1)

data1 <- data1 %>%
  group_by(group) %>%
  mutate(time = mean(time)) %>%
  ungroup() %>%
  select(-group)

data2 <- data2[order(data2$time), ]
data2 <- data2 %>%
  mutate(group = (row_number() - 1) %/% 5 + 1)

data2 <- data2 %>%
  group_by(group) %>%
  mutate(time = mean(time)) %>%
  ungroup() %>%
  select(-group)

data3 <- data3[order(data3$time), ]
data3 <- data3 %>%
  mutate(group = (row_number() - 1) %/% 5 + 1)

data3 <- data3 %>%
  group_by(group) %>%
  mutate(time = mean(time)) %>%
  ungroup() %>%
  select(-group)

data <- rbind(data1, data2, data3)
data <- data[order(data$time), ]

# Original data
res.cox <- coxph(Surv(time, status) ~ age + sex + ph.ecog + ph.karno + pat.karno + meal.cal + wt.loss, dataOG)
#res.cox <- coxph(Surv(time, status) ~ X1 + X2 + X3, dataOG)
res.cox
summary(res.cox)

# Averaged data
res.cox <- coxph(Surv(time, status) ~ age + sex + ph.ecog + ph.karno + pat.karno + meal.cal + wt.loss, data)
#res.cox <- coxph(Surv(time, status) ~ X1 + X2 + X3, data)
res.cox
summary(res.cox)
 

# Define the arrays
# array1 <- c(3, 4, 6, 7, 9, 12, 14, 16, 19, 24, 35, 37, 39, 45)
# array2 <- c(8, 9, 10, 11, 14, 16, 17, 19, 22, 24, 28, 29, 30, 32, 35, 36)
# array3 <- c(1, 3, 6, 7, 9, 12, 14, 18, 23, 27, 32, 33, 35, 38, 42, 46, 47)

# TODO find a way to split all arrays in intervals (same int for all arrays)
data1 <- read.csv("Data_site_1.csv")
data2 <- read.csv("Data_site_2.csv")
data3 <- read.csv("Data_site_3.csv")

# Constant intervals - except the last one
#array1 <- c(3, 4, 6, 7, 9, 12, 14, 16, 19, 24, 35, 37, 39, 45)
data1 <- data1[order(data1$time), ]
array1 <- data1$time

differences1 <- abs(array1 - array1[c(5:length(array1), 1, 2)])
max_difference1 <- max(differences1[1:(length(differences1) - 4)])
min_array_1 <- min(array1)
max_array_1 <- max(array1)

#array2 <- c(8, 9, 10, 11, 14, 16, 17, 19, 22, 24, 28, 29, 30, 32, 35, 36)
data2 <- data2[order(data2$time), ]
array2 <- data2$time
differences2 <- abs(array2 - array2[c(5:length(array2), 1, 2)])
max_difference2 <- max(differences2[1:(length(differences2) - 4)])
min_array_2 <- min(array2)
max_array_2 <- max(array2)

#array3 <- c(1, 3, 6, 7, 9, 12, 14, 18, 23, 27, 32, 33, 35, 38, 42, 46, 47)
data3 <- data3[order(data3$time), ]
array3 <- data3$time
differences3 <- abs(array3 - array3[c(5:length(array3), 1, 2)])
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

data1 <- modify_time(data1, intervals)
data2 <- modify_time(data2, intervals)
data3 <- modify_time(data3, intervals)

dataInt <- rbind(data1, data2, data3)
dataInt <- dataInt[order(dataInt$time), ]

# Data intervals
res.cox <- coxph(Surv(time, status) ~ age + sex + ph.ecog + ph.karno + pat.karno + meal.cal + wt.loss, dataInt)
#res.cox <- coxph(Surv(time, status) ~ X1 + X2 + X3, dataInt)
res.cox
summary(res.cox)
