############### DISTRIBUTED COX MODEL ####################
############### Local site code ###########################

## License: https://creativecommons.org/licenses/by-nc-sa/4.0/
## Copyright: GRIIS / Université de Sherbrooke

# Loading packages and setting up core variables --------------------------
library("survival")
library("survminer")

# Third function --- Calculate parameters for beta
calculate_local_values <- function(man_wd=-1,nodeid=-1, nodebetas=-1, nodenumber=-1) {
  
  manualwd <- man_wd
  k <- nodeid
  nbBetas <- nodebetas
  max_number <- nodenumber
  
  if (k<0){
    stop
  } 
  
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
  
  # Read data, get event times, write in csv
  node_data <- read.csv(paste0("Data_site_", k, ".csv"))
  beta <-  read.csv(paste0("Beta_", max_number, "_output.csv"))
  Rik <- read.csv(paste0("Rik", k, ".csv"), header = FALSE, blank.lines.skip = FALSE)
  Rik <- Rik[-1, ]
  
  # Create the sumExp matrix with the same number of rows as Rik1 and 1 column
  sumExp <- numeric(nrow(Rik))
  sumZqExp <- matrix(0, nrow = nrow(Rik), ncol = nbBetas)
  sumZqZrExp <- array(0, dim = c(nbBetas, nbBetas, nrow(Rik)))
  
  # For each line in Rik1, calculate the sum of exp(transpose(beta) * z)
  for (i in 1:nrow(Rik)) {
    indices <- unlist(Rik[i, ])
    indices <- as.numeric(indices[indices != ""])
    for (j in seq_along(indices)) {
      z <- c(node_data$age[indices[j]], node_data$sex[indices[j]])
      sumExp[i] <- sumExp[i] + (exp(sum(beta * z)))
      sumZqExp[i, ] <- sumZqExp[i, ] + z * (exp(sum(beta * z)))
      sumZqZrExp[, , i] <- sumZqZrExp[, , i] + z %*% t(z) * exp(sum(beta * z))
    } 
  }
  
  # Write in csv
  write.csv(sumExp, file=paste0("sumExp",k,"_output_", max_number,".csv"),row.names = FALSE,na="")
  write.csv(sumZqExp, file=paste0("sumZqExp",k,"_output_", max_number,".csv"),row.names = FALSE,na="")
  
  # Write in csv for 3D matrix (a bit more complex than 2d)
  list_of_matrices <- lapply(seq_len(dim(sumZqZrExp)[3]), function(i) sumZqZrExp[,,i])
  list_of_vectors <- lapply(list_of_matrices, as.vector)
  combined_matrix <- do.call(cbind, list_of_vectors)
  write.csv(combined_matrix, file = paste0("sumZqZrExp",k,"_output_", max_number,".csv"), row.names = FALSE)
  
  rm(list = ls())
  
  return(TRUE)
}
