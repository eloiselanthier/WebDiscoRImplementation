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

pas <- 1
interval_size <- 1
increase <- 1
j <- 1

interval_size_OG <- interval_size

left_border <- min_total
right_border <- min_total + interval_size

nbTypesOfIntervals <- floor(((max_total-min_total) - interval_size)/increase)+1
maxNbOfIntervals <- floor(((max_total-interval_size)-min_total)/pas)+1

binary_output_site1 <- matrix(0, nrow = nbTypesOfIntervals, ncol = maxNbOfIntervals)
binary_output_site2 <- matrix(0, nrow = nbTypesOfIntervals, ncol = maxNbOfIntervals)
binary_output_site3 <- matrix(0, nrow = nbTypesOfIntervals, ncol = maxNbOfIntervals)

maxNbOfIntervals
nbTypesOfIntervals

while (interval_size < (max_total-min_total) ){
  
  i <- 1
  
  while (right_border < max_total){
    
    if(sum(data1$time >= left_border & data1$time < right_border) >= nbDataPts) {
      binary_output_site1[j,i] <- 1
    }
    if(sum(data2$time >= left_border & data2$time < right_border) >= nbDataPts) {
      binary_output_site2[j,i] <- 1
    }
    
    if(sum(data3$time >= left_border & data3$time < right_border) >= nbDataPts) {
      binary_output_site3[j,i] <- 1
    }
    
    left_border <- left_border + pas
    right_border <- right_border + pas
    i <- i + 1
    
  }
  
  interval_size <- interval_size + increase
  
  left_border <- min_total
  right_border <- min_total + interval_size
  
  j <- j + 1
}

binary_output_global <- binary_output_site1 + binary_output_site2 + binary_output_site3

intervals <- list()

position <- 1
value <- min_total
nbRows <- nrow(binary_output_global)
done <- FALSE

while (position < ncol(binary_output_global)) {
  for (i in 1:nbRows) {
    if (done == FALSE){
      if (binary_output_global[i, position] == 3) {
        value <- value + (interval_size_OG + (i-1) * increase)
        intervals <- append(intervals, value)
        position <- floor((value - min_total)/interval_size_OG)
        done <- TRUE
      }
      if (i == nbRows){ # If nothing else, stop the loop
        position <- ncol(binary_output_global)
      }
    }
  }
  done <- FALSE
}

intervals <- append(intervals,max_total+1)
# intervals <- intervals[-(length(intervals) - 1)]    # Dernier intervalle merged avec l'avant dernier

data1$time <- cut(data1$time, breaks = c(-Inf, intervals), labels = FALSE, right = FALSE)
data2$time <- cut(data2$time, breaks = c(-Inf, intervals), labels = FALSE, right = FALSE)
data3$time <- cut(data3$time, breaks = c(-Inf, intervals), labels = FALSE, right = FALSE)

# Dernier interval à check




# Données finales
dataInt <- rbind(data1, data2, data3)
dataInt <- dataInt[order(dataInt$time), ]

# Error
res.cox.OG <- coxph(Surv(time, status) ~ age + sex + ph.ecog + ph.karno + pat.karno + meal.cal + wt.loss, dataOG)
res.cox.Int <- coxph(Surv(time, status) ~ age + sex + ph.ecog + ph.karno + pat.karno + meal.cal + wt.loss, dataInt)

#res.cox.OG <- coxph(Surv(time, status) ~ X1 + X2 + X3, dataOG)
#res.cox.Int <- coxph(Surv(time, status) ~ X1 + X2 + X3, dataInt)

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













