# Includes
library("survival")
library("survminer")
library("dplyr")

data1 <- read.csv("Data_site_1.csv")
data2 <- read.csv("Data_site_2.csv")
data3 <- read.csv("Data_site_3.csv")

# Standard
dataOG <- rbind(data1, data2, data3)

# En faisant la moyenne de cinq données de temps

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
res.cox.OG <- coxph(Surv(time, status) ~ age + sex + ph.ecog + ph.karno + pat.karno + meal.cal + wt.loss, dataOG)
#res.cox.OG <- coxph(Surv(time, status) ~ X1 + X2 + X3, dataOG)
res.cox.OG
summary(res.cox.OG)

# Data intervals
res.cox.moy <- coxph(Surv(time, status) ~ age + sex + ph.ecog + ph.karno + pat.karno + meal.cal + wt.loss, data)
#res.cox.moy <- coxph(Surv(time, status) ~ X1 + X2 + X3, data)
res.cox.moy
summary(res.cox.moy)

coefficients.OG <- coef(res.cox.OG)
coefficients.moy <- coef(res.cox.moy)
differences <- coefficients.OG - coefficients.moy
percentage_errors <- (differences / coefficients.OG) * 100
absolute_errors <- abs(percentage_errors)
error <- sum(absolute_errors)
error







