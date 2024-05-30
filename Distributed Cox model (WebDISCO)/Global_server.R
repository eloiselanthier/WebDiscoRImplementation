############### DISTRIBUTED COX MODEL ####################
############### Coordinating node code ###################

## License: https://creativecommons.org/licenses/by-nc-sa/4.0/
## Copyright: GRIIS / Université de Sherbrooke

# Includes
library("survival")
library("survminer")

# If you want to skip the automated working directory setting, input 1 here. 
# If you do so, make sure the working directory is set correctly manualy.
manualwd <- -1

# Number of parameters (covariates)
nbBetas <- 3

if (manualwd != 1) {
  
  # Set working directory automatically
  
  # this.path package is available
  if (require(this.path)) {
    setwd(this.dir())
    
    # else if running in R studio and the rstudioapi is available, set the correct working directory
  } else if ((Sys.getenv("RSTUDIO") == "1") & (require("rstudioapi"))) {
    print("RSTUDIO")
    path <- dirname(rstudioapi::getActiveDocumentContext()$path)
    setwd(path)
    
    # no known means to automatically set working directory
  } else {
    stop("The required conditions to automatically set the working directory are not met. See R file")
  }
} else {
  print("The automated working directory setup has been bypassed. If there is an error, this might be the cause.")
}

# ------------------------- CODE STARTS HERE ------------------------

K=length(list.files(pattern="Times_[[:digit:]]+_output.csv"))

# First step: finding global times
if (!file.exists("Global_times_output.csv")) {
  
  times_list <- list()
  
  for (k in 1:K) {
    times_list[[k]] <- read.csv(paste0("Times_", k, "_output.csv"))
  }
  
  combined_times <- do.call(c, lapply(times_list, function(df) unlist(df)))
  Dlist <- sort(unique(combined_times))
  
  write.csv(Dlist, file="Global_times_output.csv", row.names = FALSE)

}

# Next step: global initialization and first beta
if (file.exists(paste0("Dik", K, ".csv"))) {
  
  sumZrGlobal_int <- 0
  
  for(i in 1:K){
    Dik <- read.csv(paste0("Dik", i, ".csv"), header = FALSE, blank.lines.skip = FALSE)
    if(i == 1){
      normDikGlobal <- matrix(0, nrow = nrow(Dik)-1, ncol = 1)
    }
    normDikGlobal <- normDikGlobal + apply(Dik, 1, function(row) sum(row != ""))[-1]

    sumZrh <- read.csv(paste0("sumZrh", i, ".csv"))
    sumZrGlobal_int <- sumZrGlobal_int + colSums(sumZrh)
  }

  if (!file.exists("Beta_1_output.csv")){
    beta <- rep(0, nbBetas)
    write.csv(beta, file="Beta_1_output.csv", row.names = FALSE)
  }
  
}

# Last step: Calculate derivatives and new beta
if (file.exists("Beta_1_output.csv") & file.exists("sumExp1_output_1.csv") ) {
  
  # Must use the last available data
  files <- list.files(pattern = "Beta_\\d+_output.csv")
  numbers <- as.numeric(gsub("Beta_(\\d+)_output.csv", "\\1", files))
  ite <- max(numbers)
  
  # Verification to make sure new data is used to compute beta
  if (file.exists((paste0("sumExp1_output_", ite, ".csv")))){
    
    # Get old beta
    beta <-  read.csv(paste0("Beta_", ite, "_output.csv"))
    
    # Read files and sum values
    for(i in 1:K){
      sumExp <- read.csv(paste0("sumExp", i, "_output_", ite, ".csv"), header = FALSE, blank.lines.skip = FALSE)
      sumExp <- matrix(as.numeric(as.matrix(sumExp[-1, ])), ncol = 1, byrow = FALSE)
      
      sumZqExp <- read.csv(paste0("sumZqExp", i, "_output_", ite, ".csv"), header = FALSE, blank.lines.skip = FALSE)
      sumZqExp <- matrix(as.numeric(as.matrix(sumZqExp[-1, ])), ncol = nbBetas, byrow = FALSE)
      
      sumZqZrExp <- read.csv(paste0("sumZqZrExp", i, "_output_", ite, ".csv"), header = FALSE, blank.lines.skip = FALSE)
      sumZqZrExp <- array(as.numeric(as.matrix(sumZqZrExp[-1, ])), dim = c(nbBetas, nbBetas, ncol(sumZqZrExp)))
      
      if(i == 1){
        sumExpGlobal <- matrix(0, nrow = nrow(sumExp), ncol = ncol(sumExp))
        sumZqExpGlobal <- matrix(0, nrow = nrow(sumZqExp), ncol = ncol(sumZqExp))
        sumZqZrExpGlobal <- array(0, dim = dim(sumZqZrExp))
      }
      
      sumExpGlobal <- sumExpGlobal + sumExp
      sumZqExpGlobal <- sumZqExpGlobal + sumZqExp
      sumZqZrExpGlobal <- sumZqZrExpGlobal + sumZqZrExp
    }
    
    # Calculate first derivative
    ZrExp_Exp <- sumZqExpGlobal/do.call(cbind, replicate(nbBetas, sumExpGlobal, simplify = FALSE))
    Norm_ZrExp_Exp <- do.call(cbind, replicate(nbBetas, normDikGlobal, simplify = FALSE)) * ZrExp_Exp
    sumDi_Norm_ZrExp_Exp <- colSums(Norm_ZrExp_Exp)
    
    lr_beta = sumZrGlobal_int - sumDi_Norm_ZrExp_Exp
    
    # Calculate second derivative
    lrq_beta <- matrix(NA, nrow = nbBetas, ncol = nbBetas)
    
    for (i in 1:nbBetas) {
      for (j in 1:nbBetas) {
        a <- sumZqZrExpGlobal[i, j, ] / sumExpGlobal
        b <- sumZqExpGlobal[, i] / sumExpGlobal
        c <- sumZqExpGlobal[, j] / sumExpGlobal
        
        value_ij <- a - b * c
        Norm_value_ij <- normDikGlobal * value_ij
        lrq_beta[i, j] <- -sum(Norm_value_ij)
      }
    }
    
    # Calculate new beta
    lrq_beta_inv <- solve(lrq_beta)
    betaT <- matrix(as.numeric(lr_beta), nrow = nbBetas, ncol = 1)
    
    beta <- beta - lrq_beta_inv %*% betaT
    
    # Write in csv to max_number+1
    write.csv(beta, file=paste0("Beta_", ite+1, "_output.csv"), row.names = FALSE)
    
  } else {
    print("New values must be computed locally in order to do another iteration.")
  }
}

# Clear variables
rm(list = ls())