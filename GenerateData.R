# Load required libraries
library(survival)
library(dplyr)

# Set the specific beta values
betas <- c(2, -1, 0.5) # Example betas for X1, X2, and X3

# Number of observations
N <- 150

# Generate custom covariates
set.seed(117) # For reproducibility
X1 <- rnorm(N, mean=0, sd=1)       # Normal distribution
X2 <- rbinom(N, size=1, prob=0.5)  # Binomial distribution
X3 <- runif(N, min=-1, max=1)      # Uniform distribution

# Combine into a data frame
covariates <- data.frame(X1, X2, X3)

# Calculate the linear predictor (eta)
eta <- betas[1] * X1 + betas[2] * X2 + betas[3] * X3

# Generate baseline hazard (constant for simplicity)
baseline_hazard <- 0.1

# Generate survival times
survival_times <- rexp(N, rate = baseline_hazard * exp(eta))

# Generate censoring times (for example, uniformly between 0 and 100)
censoring_times <- runif(N, min = 0, max = 100)

# Determine observed times and event indicator
observed_times <- pmin(survival_times, censoring_times)
event_indicator <- as.numeric(survival_times <= censoring_times)

# Combine into a data frame
simdata <- data.frame(
  time = observed_times,
  status = event_indicator,
  X1 = X1,
  X2 = X2,
  X3 = X3
)

# Inspect the first 10 rows of the generated data
head(simdata, 10)

# Fit a Cox proportional hazards model to the generated data
model <- coxph(Surv(time, status) ~ X1 + X2 + X3, data = simdata)

# Display the coefficients of the fitted model
model$coefficients

write.csv(simdata, "Data_site_3.csv", row.names = FALSE)
