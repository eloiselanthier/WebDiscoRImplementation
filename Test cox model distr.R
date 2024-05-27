# Includes
library("survival")
library("survminer")

# ON RECHERCHE BETA = (0.011, -0.553) OU (0.019, -0.53) OU (0.017, -0.513) je suis pas certaine

# Data
data("lung")
head(lung)

res.cox <- coxph(Surv(time, status) ~ age + sex, data = lung)
res.cox
summary(res.cox)

# Create a factor variable indicating the desired intervals
lung$inst_class <- cut(lung$inst, breaks = c(0, 5, 13, 33), labels = c("1-5", "6-13", "14-33"), include.lowest = TRUE)

# Specify the columns to keep
columns_to_keep <- c("time", "status", "age", "sex")    # We keep only age and sex to simplify calculations (only 2 betas)

# Split the data into three classes based on the intervals and keep only specified columns
hospital1 <- lung[lung$inst_class == "1-5", columns_to_keep]
hospital2 <- lung[lung$inst_class == "6-13", columns_to_keep]
hospital3 <- lung[lung$inst_class == "14-33", columns_to_keep]

# View the first few rows of each class
head(hospital1)
head(hospital2)
head(hospital3)

# Local initialization for all sites (TODO in another file eventually)

times1 <- unique(hospital1[, 1])
sorted_times1 <- sort(times1)
# Times when patients die (excludes censored data)
times1D <- unique(hospital1$time[hospital1$status == 2]) # Equivalent to D "local"
sorted_times1D <- sort(times1D)

times2 <- unique(hospital2[, 1])
sorted_times2 <- sort(times2)
# Times when patients die (excludes censored data)
times2D <- unique(hospital2$time[hospital2$status == 2])
sorted_times2D <- sort(times2D)

times3 <- unique(hospital3[, 1])
sorted_times3 <- sort(times3)
# Times when patients die (excludes censored data)
times3D <- unique(hospital3$time[hospital3$status == 2])
sorted_times3D <- sort(times3D)

# Initialization D global (différent de l'article mais bon)
Dlist <- sort(unique(c(times1, times2, times3))) # D inclus tous les temps ou juste les deces? Pour l'instant, tout
D <- length(Dlist)

# ---------- SITE 1 ---------------------------------

# Dik - list of patients that have died at all times i for hospital 1
Dik1 <- vector("list", length(Dlist))

for (i in seq_along(Dlist)) {
  Dik1[[i]] <- which(hospital1$time <= Dlist[i] & hospital1$status == 2) # On ajoute à Di si décès (status = 2)
}

normDik1 <- sapply(Dik1, length)

# Rik - list of patients that are still at risk at all times i for hospital 1
Rik1 <- vector("list", length(Dlist))

# For each sorted time value, find the row indices where time is greater (last line is empty, problem ?)
for (i in seq_along(Dlist)) {
  Rik1[[i]] <- which(hospital1$time > Dlist[i])
}

# Sum of zr

# Initialize the matrix sumZrh1 with the same number of rows as Dik1 and 2 columns for age and sex sums (number of betas)
sumZrh1 <- matrix(0, nrow = length(Dik1), ncol = 2)

# For each line in Dik1, calculate the sum of age and sex for the corresponding indices in hospital1
for (i in seq_along(Dik1)) {
  indices <- Dik1[[i]]
  sumZrh1[i, 1] <- sum(hospital1$age[indices])
  sumZrh1[i, 2] <- sum(hospital1$sex[indices])
}

# -------------------- SITE 2 --------------------------------

Dik2 <- vector("list", length(Dlist))

for (i in seq_along(Dlist)) {
  Dik2[[i]] <- which(hospital2$time <= Dlist[i] & hospital2$status == 2)
}

normDik2 <- sapply(Dik2, length)

# Rik - list of patients that are still at risk at all times i for hospital 1
Rik2 <- vector("list", length(Dlist))

# For each sorted time value, find the row indices where time is greater (last line is empty, problem ?)
for (i in seq_along(Dlist)) {
  Rik2[[i]] <- which(hospital2$time > Dlist[i])
}

# Initialize the matrix sumZrh1 with the same number of rows as Dik1 and 2 columns for age and sex sums (number of betas)
sumZrh2 <- matrix(0, nrow = length(Dik2), ncol = 2)

# For each line in Dik1, calculate the sum of age and sex for the corresponding indices in hospital1
for (i in seq_along(Dik2)) {
  indices <- Dik2[[i]]
  sumZrh2[i, 1] <- sum(hospital2$age[indices])
  sumZrh2[i, 2] <- sum(hospital2$sex[indices])
}

# -------------------- SITE 3 -------------------------------

Dik3 <- vector("list", length(Dlist))

for (i in seq_along(Dlist)) {
  Dik3[[i]] <- which(hospital3$time <= Dlist[i] & hospital3$status == 2)
}

normDik3 <- sapply(Dik3, length)

# Rik - list of patients that are still at risk at all times i for hospital 1
Rik3 <- vector("list", length(Dlist))

# For each sorted time value, find the row indices where time is greater (last line is empty, problem ?)
for (i in seq_along(Dlist)) {
  Rik3[[i]] <- which(hospital3$time > Dlist[i])
}

# Initialize the matrix sumZrh1 with the same number of rows as Dik1 and 2 columns for age and sex sums (number of betas)
sumZrh3 <- matrix(0, nrow = length(Dik3), ncol = 2)

# For each line in Dik1, calculate the sum of age and sex for the corresponding indices in hospital1
for (i in seq_along(Dik3)) {
  indices <- Dik3[[i]]
  sumZrh3[i, 1] <- sum(hospital3$age[indices])
  sumZrh3[i, 2] <- sum(hospital3$sex[indices])
}

# Global initialization
# Dlist <- unique(c(times1, times2, times3))
# D <- length(Dlist)

sumZrGlobal <- sumZrh1 + sumZrh2 + sumZrh3
sumZrGlobal_int <- colSums(sumZrGlobal)

beta <- c(0.1, 0.2)   # Cheap initialization, eventually calculate local betas to initialize

# Loop start
for (i in 1:5) {
  # Locally
  # -------------------- SITE 1 -------------------------------
  # Calculate aggregated statistics
  
  # Create the sumExp matrix with the same number of rows as Rik1 and 1 column
  sumExp1 <- numeric(length(Rik1))
  sumZqExp1 <- matrix(0, nrow = length(Rik1), ncol = 2)
  sumZqZrExp1 <- array(0, dim = c(2, 2, length(Rik1)))
  
  # For each line in Rik1, calculate the sum of exp(transpose(beta) * z)
  for (i in seq_along(Rik1)) {
    indices <- Rik1[[i]]
    for (j in seq_along(indices)) {
      z <- c(hospital1$age[indices[j]], hospital1$sex[indices[j]])
      sumExp1[i] <- sumExp1[i] + (exp(sum(beta * z)))
      sumZqExp1[i, ] <- sumZqExp1[i, ] + z * (exp(sum(beta * z)))
      sumZqZrExp1[, , i] <- sumZqZrExp1[, , i] + z %*% t(z) * exp(sum(beta * z))
    } 
  }
  
  # -------------------- SITE 2 -------------------------------
  # Calculate aggregated statistics
  
  # Create the sumExp matrix with the same number of rows as Rik2 and 1 column
  sumExp2 <- numeric(length(Rik2))
  sumZqExp2 <- matrix(0, nrow = length(Rik2), ncol = 2)
  sumZqZrExp2 <- array(0, dim = c(2, 2, length(Rik2)))
  
  # For each line in Rik2, calculate the sum of exp(transpose(beta) * z)
  for (i in seq_along(Rik2)) {
    indices <- Rik2[[i]]
    for (j in seq_along(indices)) {
      z <- c(hospital2$age[indices[j]], hospital2$sex[indices[j]])
      sumExp2[i] <- sumExp2[i] + (exp(sum(beta * z)))
      sumZqExp2[i, ] <- sumZqExp2[i, ] + z * (exp(sum(beta * z)))
      sumZqZrExp2[, , i] <- sumZqZrExp2[, , i] + z %*% t(z) * exp(sum(beta * z))
    } 
  }
  
  # -------------------- SITE 3 -------------------------------
  # Calculate aggregated statistics
  
  # Create the sumExp matrix with the same number of rows as Rik3 and 1 column
  sumExp3 <- numeric(length(Rik3))
  sumZqExp3 <- matrix(0, nrow = length(Rik3), ncol = 2)
  sumZqZrExp3 <- array(0, dim = c(2, 2, length(Rik3)))
  
  # For each line in Rik3, calculate the sum of exp(transpose(beta) * z)
  for (i in seq_along(Rik3)) {
    indices <- Rik3[[i]]
    for (j in seq_along(indices)) {
      z <- c(hospital3$age[indices[j]], hospital3$sex[indices[j]])
      sumExp3[i] <- sumExp3[i] + (exp(sum(beta * z)))
      sumZqExp3[i, ] <- sumZqExp3[i, ] + z * (exp(sum(beta * z)))
      sumZqZrExp3[, , i] <- sumZqZrExp3[, , i] + z %*% t(z) * exp(sum(beta * z))
    } 
  }
  
  # Global server
  # Calculate first derivative
  
  sumExpGlobal <- sumExp1 + sumExp2 + sumExp3
  sumZqExpGlobal <- sumZqExp1 + sumZqExp2 + sumZqExp3
  sumZqZrExpGlobal <- sumZqZrExp1 + sumZqZrExp2 + sumZqZrExp3
  normDikGlobal <- normDik1 + normDik2 + normDik3
  
  ZrExp_Exp <- sumZqExpGlobal/cbind(sumExpGlobal, sumExpGlobal)
  Norm_ZrExp_Exp <- cbind(normDikGlobal,normDikGlobal)  * ZrExp_Exp
  sumDi_Norm_ZrExp_Exp <- colSums(Norm_ZrExp_Exp[1:184, ])
  
  lr_beta = sumZrGlobal_int - sumDi_Norm_ZrExp_Exp
  
  # Calculate second derivative
  
  # Pour 1,1
  
  a <- sumZqZrExpGlobal[1,1,] / sumExpGlobal
  b <- sumZqExpGlobal[,1] / sumExpGlobal
  c <- sumZqExpGlobal[,1] / sumExpGlobal
  
  value11 <- a - b * c
  Norm_value11 <- normDikGlobal * value11
  sumDi_Norm_Value11 <- -sum(Norm_value11[1:184])
  
  # Pour 1,2 et 2,1
  
  a <- sumZqZrExpGlobal[1,2,] / sumExpGlobal
  b <- sumZqExpGlobal[,1] / sumExpGlobal
  c <- sumZqExpGlobal[,2] / sumExpGlobal
  
  value12 <- a - b * c
  Norm_value12 <- normDikGlobal * value12
  sumDi_Norm_Value12 <- -sum(Norm_value12[1:184])
  
  # Pour 2,2
  
  a <- sumZqZrExpGlobal[2,2,] / sumExpGlobal
  b <- sumZqExpGlobal[,2] / sumExpGlobal
  c <- sumZqExpGlobal[,2] / sumExpGlobal
  
  value22 <- a - b * c
  Norm_value22 <- normDikGlobal * value22
  sumDi_Norm_Value22 <- -sum(Norm_value22[1:184])
  
  # Create a 2x2 matrix
  lrq_beta <- matrix(NA, nrow = 2, ncol = 2)
  
  # Assign values to the matrix
  lrq_beta[1, 1] <- sumDi_Norm_Value11
  lrq_beta[1, 2] <- sumDi_Norm_Value12
  lrq_beta[2, 1] <- sumDi_Norm_Value12
  lrq_beta[2, 2] <- sumDi_Norm_Value22
  
  # Update beta
  
  lrq_beta_inv <- solve(lrq_beta) # inverse
  betaT <- matrix(NA, nrow = 2, ncol = 1)
  betaT[1,1] <- lr_beta[1]
  betaT[2,1] <- lr_beta[2]
  
  beta <- beta - lrq_beta_inv %*% betaT
  print(beta)
}

# Loop stop


# Clear variables
#rm(list = ls())












