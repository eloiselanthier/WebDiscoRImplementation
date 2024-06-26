# Includes
library("survival")
library("survminer")
library("dplyr")
library("discSurv")

# Read files
data1 <- read.csv("/home/griis/Documents/Cox model R/Data privacy tests/Données simulées - Analyses/80/Continues/Data_site_1.csv")
data2 <- read.csv("/home/griis/Documents/Cox model R/Data privacy tests/Données simulées - Analyses/80/Continues/Data_site_2.csv")
data3 <- read.csv("/home/griis/Documents/Cox model R/Data privacy tests/Données simulées - Analyses/80/Continues/Data_site_3.csv")
data4 <- read.csv("/home/griis/Documents/Cox model R/Data privacy tests/Données simulées - Analyses/80/Continues/Data_site_4.csv")
data5 <- read.csv("/home/griis/Documents/Cox model R/Data privacy tests/Données simulées - Analyses/80/Continues/Data_site_5.csv")

dataOG <- rbind(data1, data2, data3, data4, data5)

data1 <- read.csv("/home/griis/Documents/Cox model R/Data privacy tests/Données simulées - Analyses/80/Discrètes/Data_site_1.csv")
data2 <- read.csv("/home/griis/Documents/Cox model R/Data privacy tests/Données simulées - Analyses/80/Discrètes/Data_site_2.csv")
data3 <- read.csv("/home/griis/Documents/Cox model R/Data privacy tests/Données simulées - Analyses/80/Discrètes/Data_site_3.csv")
data4 <- read.csv("/home/griis/Documents/Cox model R/Data privacy tests/Données simulées - Analyses/80/Discrètes/Data_site_4.csv")
data5 <- read.csv("/home/griis/Documents/Cox model R/Data privacy tests/Données simulées - Analyses/80/Discrètes/Data_site_5.csv")

dataRounded <- rbind(data1, data2, data3, data4, data5)
dataRounded$time[dataRounded$time == 0] <- 1              # Peut pas avoir d'événement au temps 0

# Discrete-time Cox
dataRoundedLong <- dataLong(dataRounded, timeColumn = "time", eventColumn = "status")
estGlm <- glm(formula = y ~ timeInt + X1 + X2 + X3 + X4 + X5 + X6 + X7, data=dataRoundedLong, family = binomial())

# Continous Cox (standard)
#res.cox.OG <- coxph(Surv(time, status) ~ X1 + X2 + X3 + X4 + X5 + X6 + X7 + X8 + X9 + X10, dataOG)
#res.cox.Rounded <- coxph(Surv(time, status) ~ X1 + X2 + X3 + X4 + X5 + X6 + X7, dataRounded)



# CALCUL DE L'ERREUR

#differences <- coef(res.cox.OG) - coef(res.cox.Rounded)
#error <- mean(abs((differences / coef(res.cox.OG)) * 100))
#error

# coefficientsGLM <- coef(estGlm)[-c(1, 2)]
# differences <- coefficientsGLM - coef(res.cox.Rounded)
# error <- mean(abs((differences / coefficientsGLM) * 100))
# error