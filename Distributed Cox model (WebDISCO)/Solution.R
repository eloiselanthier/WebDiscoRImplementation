# Includes
library("survival")
library("survminer")

data1 <- read.csv("Data_site_1.csv")
data2 <- read.csv("Data_site_2.csv")
#data3 <- read.csv("Data_site_3.csv")

data <- rbind(data1, data2)

res.cox <- coxph(Surv(time, status) ~ age+beck+ndrugtx+race+treat+site+HU+CU+IVDUPN+IVDURN, data) # Input the column names here separated by a + [x1 + x2 + x3 for example] 
summary(res.cox)



#data1 <- read.csv("Data_site_1.csv")
#data2 <- read.csv("Data_site_2.csv")
#
#test1 <- data1[,1:5]
#test2 <- data2[,1:5]
#
#write.csv(test1, file="Data_site_1.csv", row.names = FALSE)
#write.csv(test2, file="Data_site_2.csv", row.names = FALSE)
