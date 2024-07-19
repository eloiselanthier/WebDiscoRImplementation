############### DATA GENERATION FOR SURVIVAL ANALYSIS ####################

## Code inspired by: https://stats.stackexchange.com/questions/135124/how-to-create-a-toy-survival-time-to-event-data-with-right-censoring

# Loading packages and setting up core variables --------------------------
library("survival")          # Contains the core survival analysis routines   

# If you want to skip the automated working directory setting, input 1 here. 
# If you do so, make sure the working directory is set correctly manualy.
manualwd <- -1

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

# Baseline hazard: Weibull
# N = sample size    
# lambda = scale parameter in h0()
# rho = shape parameter in h0()
# beta = fixed effect parameter
# rateC = rate parameter of the exponential distribution of C

simulWeib <- function(N, lambda, rho, betas, rateC)
{
  # Covariates
  x1 <- sample(x=c(0, 1), size=N, replace=TRUE, prob=c(0.5, 0.5))
  x2 <- rnorm(N, mean=0, sd=0.5)
  x3 <- rbinom(N, size=1, prob=0.3)
  x4 <- runif(N, min=0, max=0.5)
  
  # Weibull latent event times
  v <- runif(n=N)
  Tlat <- (- log(v) / (lambda * exp(x1 * betas[1] + x2 * betas[2] + x3 * betas[3] + x4 * betas[4])))^(1 / rho)
  
  # Censoring times
  C <- rexp(n=N, rate=rateC)
  
  # Follow-up times and event indicators
  time <- pmin(Tlat, C)
  status <- as.numeric(Tlat <= C)
  
  # Data set
  data.frame(time=round(time),
             status=status,
             x1=x1,
             x2=x2,
             x3=x3,
             x4=x4)
}

set.seed(1235)
betas <- c(-0.6, 0.5, -0.4, 0.3)

dat <- simulWeib(N=100000, lambda=0.01, rho=1, betas=betas, rateC=0.001)
fit <- coxph(Surv(time, status) ~ x1 + x2 + x3 + x4, data=dat)
coef(fit)

## Remove all environment variables. 
## If you want to see the variable that were created, simply don't execute that line (and clear them manually after)
rm(list = ls())
