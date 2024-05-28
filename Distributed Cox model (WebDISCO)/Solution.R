# Includes
library("survival")
library("survminer")

data1 <- read.csv("Data_site_1.csv")
data2 <- read.csv("Data_site_2.csv")
data3 <- read.csv("Data_site_3.csv")

data <- rbind(data1, data2, data3)
data

res.cox <- coxph(Surv(time, status) ~ age + sex + ph.ecog + ph.karno + pat.karno + meal.cal + wt.loss, data)
res.cox
summary(res.cox)