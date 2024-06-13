# Includes
library("survival")
library("survminer")
library("dplyr")

fileNb <- 2
varname <- paste("data", fileNb, sep = "") # Equivalent to data1

dataCSV <- read.csv(paste("Data_site_", fileNb, ".csv", sep = ""))
assign(varname, dataCSV) # data1 <- dataCSV
dataOG <- dataCSV

# Initialize Parameters
lowcutoff <- 1.368301
cutoff <- 13.96353

# Functions
create_intervals <- function(min_array, max_array, max_difference) {
  intervals <- seq(from = min_array, to = max_array, by = max_difference)
  if (length(intervals) == 1) {
    intervals <- c(min_array, max_array)
  }
  return(intervals)
}

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

calculate_error <- function(lowcutoff, cutoff) {
  conditions <- list(
    list(condition = function(data) data[data$time <= lowcutoff, ]),
    list(condition = function(data) data[data$time > lowcutoff & data$time <= cutoff, ]),
    list(condition = function(data) data[data$time > cutoff, ])
  )
  
  data_segments <- lapply(conditions, function(cond) {
    data_segment <- cond$condition(get(varname))
    data_segment <- data_segment[order(data_segment$time), ]
    array1 <- data_segment$time
    
    if (length(array1) >= 5) {
      differences1 <- abs(array1 - c(array1[c(5:length(array1), 0, 0)], array1[(length(array1) - 3):length(array1)]))
      differences1 <- differences1[1:(length(differences1) - 4)]
    }
    else {
      differences1 <- max(array1) - min(array1)
    }
    
    intervals <- create_intervals(min(array1), max(array1), max(differences1))
    modified_data <- modify_time(data_segment, intervals)
    
    return(modified_data)
  })
  
  assign(varname, do.call(rbind, data_segments)) # data1 <- do.call(rbind, data_segments)
  
  #res.cox.OG <- coxph(Surv(time, status) ~ age + sex + ph.ecog + ph.karno + pat.karno + meal.cal + wt.loss, dataOG)
  #res.cox.Int <- coxph(Surv(time, status) ~ age + sex + ph.ecog + ph.karno + pat.karno + meal.cal + wt.loss, get(varname))
  
  res.cox.OG <- coxph(Surv(time, status) ~ X1 + X2 + X3, dataOG)
  res.cox.Int <- coxph(Surv(time, status) ~ X1 + X2 + X3, get(varname))
  
  coefficients.OG <- coef(res.cox.OG)
  coefficients.Int <- coef(res.cox.Int)
  differences <- coefficients.OG - coefficients.Int
  percentage_errors <- (differences / coefficients.OG) * 100
  absolute_errors <- abs(percentage_errors)
  error <- sum(absolute_errors)
  
  return(error)
}

# Simulated Annealing Algorithm
optimize_parameters_sa <- function(initial_lowcutoff, initial_cutoff, max_iterations = 1500, initial_temp = 1000, cooling_rate = 0.98, initial_step_size = 0.1) {
  best_lowcutoff <- initial_lowcutoff
  best_cutoff <- initial_cutoff
  best_error <- calculate_error(best_lowcutoff, best_cutoff)
  current_lowcutoff <- best_lowcutoff
  current_cutoff <- best_cutoff
  current_error <- best_error
  temperature <- initial_temp
  step_size <- initial_step_size
  
  for (iteration in 1:max_iterations) {
    # Generate new candidate solution
    step_size <- step_size * 0.998
    new_lowcutoff <- current_lowcutoff + sample(c(-step_size, 0, step_size), 1)
    new_cutoff <- current_cutoff + sample(c(-step_size, 0, step_size), 1)
    
    # Ensure the new_cutoff is always greater than new_lowcutoff
    if (new_lowcutoff >= new_cutoff || new_lowcutoff < 0.1) {
      next
    }
    
    print(new_lowcutoff)
    print(new_cutoff)
    new_error <- calculate_error(new_lowcutoff, new_cutoff)
    
    if (new_error < current_error || runif(1) < exp((current_error - new_error) / temperature)) {
      current_lowcutoff <- new_lowcutoff
      current_cutoff <- new_cutoff
      current_error <- new_error
      
      if (current_error < best_error) {
        best_lowcutoff <- current_lowcutoff
        best_cutoff <- current_cutoff
        best_error <- current_error
      }
    }
    
    # Decrease the temperature
    temperature <- temperature * cooling_rate
  }
  
  return(list(lowcutoff = best_lowcutoff, cutoff = best_cutoff, error = best_error))
}

# Run Optimization with Simulated Annealing
result_sa <- optimize_parameters_sa(lowcutoff, cutoff)
print(result_sa)
