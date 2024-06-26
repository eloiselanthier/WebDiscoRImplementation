# Load required libraries
library(survival)
library(dplyr)

# Set the specific beta values (for 10 parameters)
betas <- c(1, 0.5, 0.3, 0.1, -0.2, 0.05, 0.1, 0.25, -0.15, 0.4)

# Number of observations
N <- 10000  # Increase number of observations for better spread

# Generate custom covariates
set.seed(129)

# Generate 10 covariates with different distributions
X1 <- rgamma(N, shape=2, scale=1)
X2 <- rpois(N, lambda=2)
X3 <- rnorm(N, mean=3, sd=1)
X4 <- rbinom(N, size=1, prob=0.3)
X5 <- runif(N, min=1, max=2)
X6 <- rnorm(N, mean=5, sd=2)
X7 <- rbinom(N, size=1, prob=0.3)
X8 <- rexp(N, rate=10)
X9 <- rnorm(N, mean=-1, sd=0.3)
X10 <- rbinom(N, size=1, prob=0.7)

# Combine into a data frame
covariates <- data.frame(X1, X2, X3, X4, X5, X6, X7, X8, X9, X10)

# Calculate the linear predictor (eta) for 10 parameters
eta <- betas[1] * X1 + betas[2] * X2 + betas[3] * X3 + betas[4] * X4 + betas[5] * X5 +
  betas[6] * X6 + betas[7] * X7 + betas[8] * X8 + betas[9] * X9 + betas[10] * X10

# Baseline hazard (constant for simplicity)
baseline_hazard <- 0.0052  # Reduced to spread out the survival times

# Generate survival times
survival_times <- rexp(N, rate = baseline_hazard * exp(eta)) * 27  # Increased multiplier for spread

shift_factor <- 11  # Adjust this factor to move the peak
transformed_survival_times <- survival_times + shift_factor

# Generate censoring times (for example, uniformly between 0 and 100)
censoring_times <- runif(N, min = 0, max = 100)

# Determine observed times and event indicator
observed_times <- pmin(transformed_survival_times, censoring_times)
event_indicator <- as.numeric(transformed_survival_times <= censoring_times)

# Combine into a data frame
simdata <- data.frame(
  time = observed_times,
  status = event_indicator,
  X1 = X1,
  X2 = X2,
  X3 = X3,
  X4 = X4,
  X5 = X5,
  X6 = X6,
  X7 = X7,
  X8 = X8,
  X9 = X9,
  X10 = X10
)

# Inspect the first 10 rows of the generated data
head(simdata, 10)

# Fit a Cox proportional hazards model to the generated data
model <- coxph(Surv(time, status) ~ X1 + X2 + X3 + X4 + X5 + X6 + X7 + X8 + X9 + X10, data = simdata)

# Display the coefficients of the fitted model
model$coefficients

# Save the generated data to a CSV file
write.csv(simdata, "Data_site_5.csv", row.names = FALSE)
