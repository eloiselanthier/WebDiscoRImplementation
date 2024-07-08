# Includes
library("survival")
library("survminer")

data1 <- read.csv("Data_site_1.csv")
data2 <- read.csv("Data_site_2.csv")
data3 <- read.csv("Data_site_3.csv")

data <- rbind(data1, data2, data3)

res.cox <- coxph(Surv(time, status) ~ X1 + X2 + X3, data)
summary(res.cox)