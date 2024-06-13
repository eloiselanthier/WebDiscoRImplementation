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
nbDataPts <- 4

# Find min and max values
data1 <- data1[order(data1$time), ]
data2 <- data2[order(data2$time), ]
data3 <- data3[order(data3$time), ]

length(data1$time)
length(data2$time)
length(data3$time)

min_total <- min(min(data1$time), min(data2$time), min(data3$time))
max_total <- max(max(data1$time), max(data2$time), max(data3$time))