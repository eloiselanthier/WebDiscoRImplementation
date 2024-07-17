# Includes
library("survival")
library("survminer")

data1 <- read.csv("Data_site_1.csv")
data2 <- read.csv("Data_site_2.csv")
#data3 <- read.csv("Data_site_3.csv")

nbBetas <- 3 # Input the number of betas

data <- rbind(data1, data2)

column_indices <- (3:(nbBetas + 2))
formula <- as.formula(paste("Surv(time, status) ~", paste(paste0("data[,", column_indices, "]"), collapse = " + ")))
res.cox <- coxph(formula, data)
summary(res.cox)



#data1 <- read.csv("Data_site_1.csv")
#data2 <- read.csv("Data_site_2.csv")
#
#test1 <- data1[,1:5]
#test2 <- data2[,1:5]
#
#write.csv(test1, file="Data_site_1.csv", row.names = FALSE)
#write.csv(test2, file="Data_site_2.csv", row.names = FALSE)
