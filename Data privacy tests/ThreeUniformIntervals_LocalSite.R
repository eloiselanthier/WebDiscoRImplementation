# Includes
library("survival")
library("survminer")
library("dplyr")

fileNb <- 4
varname <- paste("data", fileNb, sep = "") # Equivalent do data1

dataCSV <- read.csv(paste("Data_site_", fileNb, ".csv", sep = ""))
assign(varname, dataCSV) # data1 <- dataCSV
dataOG <- dataCSV

# Variables - needs to be automated
lowcutoff <- 8
cutoff <- 19

# --------- FUNCTIONS NEEDED ------------

# Calculate intervals
create_intervals <- function(min_array, max_array, max_difference) {
  intervals <- seq(from = min_array, to = max_array, by = max_difference)
  if (max_array - intervals[length(intervals)] < max_difference) {
    intervals[length(intervals)] <- max_array
  }
  return(intervals)
}

# Modify time in data frame
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

# --------- Determine intervals (data split into 3) ------------

conditions <- list(
  list(condition = function(data) data[data$time <= lowcutoff, ]),
  list(condition = function(data) data[data$time > lowcutoff & data$time <= cutoff, ]),
  list(condition = function(data) data[data$time > cutoff, ])
)

# Process data for each condition
data_segments <- lapply(conditions, function(cond) {
  data_segment <- cond$condition(get(varname))
  data_segment <- data_segment[order(data_segment$time), ]
  array1 <- data_segment$time
  
  differences1 <- abs(array1 - c(array1[c(5:length(array1), 0, 0)], array1[(length(array1) - 3):length(array1)]))
  
  intervals <- create_intervals(min(array1), max(array1), max(differences1[1:(length(differences1) - 4)]))
  modified_data <- modify_time(data_segment, intervals)
  
  return(modified_data)
})

# Merge different intervals
assign(varname, do.call(rbind, data_segments)) #data1 <- do.call(rbind, data_segments)

# ------- JUSTE POUR VOIR, NE VAUT RIEN DIRE CONCRETEMENT -----------
# Original data
#res.cox.OG <- coxph(Surv(time, status) ~ age + sex + ph.ecog + ph.karno + pat.karno + meal.cal + wt.loss, dataOG)
res.cox.OG <- coxph(Surv(time, status) ~ X1 + X2 + X3, dataOG)

# Data intervals
#res.cox.Int <- coxph(Surv(time, status) ~ age + sex + ph.ecog + ph.karno + pat.karno + meal.cal + wt.loss, get(varname))
res.cox.Int <- coxph(Surv(time, status) ~ X1 + X2 + X3, get(varname))

# Sum de l'erreur relative
coefficients.OG <- coef(res.cox.OG)
coefficients.Int <- coef(res.cox.Int)
differences <- coefficients.OG - coefficients.Int
percentage_errors <- (differences / coefficients.OG) * 100
absolute_errors <- abs(percentage_errors)
error <- sum(absolute_errors)
error