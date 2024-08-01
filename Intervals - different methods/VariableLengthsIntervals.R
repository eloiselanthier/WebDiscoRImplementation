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

length(data1$time)
length(data2$time)
length(data3$time)

min_total <- min(min(data1$time), min(data2$time), min(data3$time))
max_total <- max(max(data1$time), max(data2$time), max(data3$time))

# Define initial interval size
intervalIncrement <- 0.001
intervalStart <- min_total
intervalEnd <- intervalStart

#intervalNb <- 0
intervalAvg <- (intervalStart + intervalEnd)/2
intervalOld <- intervalAvg

intervalStartOld <- intervalStart
intervalStartOld2 <- intervalStart

while(intervalEnd < max_total){
  validSite1 <- FALSE
  validSite2 <- FALSE
  validSite3 <- FALSE
  
  #intervalNb <- intervalNb + 1
  
  while((validSite1 == FALSE || validSite2 == FALSE || validSite3 == FALSE) && intervalEnd <= max_total) {
    intervalEnd <- intervalEnd + intervalIncrement
    
    # On regarde s'il y a 5 valeurs dans l'intervalle
    if(sum(data1$time >= intervalStart & data1$time < intervalEnd) >= nbDataPts) {
      validSite1 <- TRUE
    }
    
    if(sum(data2$time >= intervalStart & data2$time < intervalEnd) >= nbDataPts) {
      validSite2 <- TRUE
    }
    
    if(sum(data3$time >= intervalStart & data3$time < intervalEnd) >= nbDataPts) {
      validSite3 <- TRUE
    }
  }
  
  intervalOld <- intervalAvg
  print(intervalOld)
  intervalAvg <- (intervalStart + intervalEnd)/2
  
  # On remplace les valeurs de temps dans l'intervalle par la valeur moyenne
  data1$time[data1$time >= intervalStart & data1$time < intervalEnd] <- intervalAvg
  data2$time[data2$time >= intervalStart & data2$time < intervalEnd] <- intervalAvg
  data3$time[data3$time >= intervalStart & data3$time < intervalEnd] <- intervalAvg
  
  intervalStartOld2 <- intervalStartOld
  intervalStartOld <- intervalStart
  
  intervalStart <- intervalEnd
}

# Adjustments for the last interval (pour 4 datapts, ajouter ça réduit l'erreur, mais pour 5 datapts, ça augmente de bcp l'erreur)
#data1$time[data1$time == intervalAvg] <- intervalOld
#data2$time[data2$time == intervalAvg] <- intervalOld
#data3$time[data3$time == intervalAvg] <- intervalOld
#
#intervalFinal <- (intervalStartOld2 + intervalEnd)/2
#
#data1$time[data1$time == intervalOld] <- intervalFinal
#data2$time[data2$time == intervalOld] <- intervalFinal
#data3$time[data3$time == intervalOld] <- intervalFinal

# Données finales
dataInt <- rbind(data1, data2, data3)
dataInt <- dataInt[order(dataInt$time), ]

# Error
res.cox.OG <- coxph(Surv(time, status) ~ age + sex + ph.ecog + ph.karno + pat.karno + meal.cal + wt.loss, dataOG)
res.cox.Int <- coxph(Surv(time, status) ~ age + sex + ph.ecog + ph.karno + pat.karno + meal.cal + wt.loss, dataInt)

#res.cox.OG <- coxph(Surv(time, status) ~ X1 + X2 + X3, dataOG)
#res.cox.Int <- coxph(Surv(time, status) ~ X1 + X2 + X3, dataInt)

#res.cox.OG <- coxph(Surv(time, status) ~ X1 + X2 + X3 + X4 + X5 + X6, dataOG)
# <- coxph(Surv(time, status) ~ X1 + X2 + X3 + X4 + X5 + X6, dataInt)

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
