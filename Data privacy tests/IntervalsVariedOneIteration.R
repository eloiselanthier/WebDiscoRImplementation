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

intervals <- list(min_total)

position <- 1
nbRows <- nrow(binary_output_global)

while (position < max_total) {
  for (i in 1:nbRows) {
    if (binary_output_global[i, position] == 3) {
      intervals <- append(intervals, binary_output_global[i, position])
      position <- position + (interval_size + i * increase)
      break
    }
    # If nothing else, stop the loop
    position <- max_total
  }
}

intervals <- append(intervals,max_total)

# À revérifier... dans intervals, en ce moment c'est pas les valeurs, c'est les positions
# Si pas est pas de 1, à checker aussi


















