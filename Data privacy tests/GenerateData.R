# Load required libraries
library(survival)
library(dplyr)

# Set the specific beta values
betas <- c(2, -1, 0.5, 0.032, 1.5, -0.017) # Example betas for X1, X2, and X3

# Number of observations
N <- 10000

# Generate custom covariates
set.seed(122) # For reproducibility
X1 <- rnorm(N, mean=0, sd=5)       # Normal distribution
X2 <- rbinom(N, size=1, prob=0.7)  # Binomial distribution
X3 <- runif(N, min=-1, max=1)      # Uniform distribution
X4 <- rnorm(N, mean=0, sd=2)       # Normal distribution
X5 <- rbinom(N, size=1, prob=0.2)  # Binomial distribution
X6 <- runif(N, min=-1, max=1)      # Uniform distribution

# Combine into a data frame
covariates <- data.frame(X1, X2, X3, X4, X5, X6)

# Calculate the linear predictor (eta)
eta <- betas[1] * X1 + betas[2] * X2 + betas[3] * X3 + betas[4] * X4 + betas[5] * X5 + betas[6] * X6

# Baseline hazard (constant for simplicity)
baseline_hazard <- 0.00005

# Generate survival times
survival_times <- rexp(N, rate = baseline_hazard * exp(eta))

# Transformation to shift the peak of the survival time distribution
shift_factor <- 6  # Adjust this factor to move the peak
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
  X6 = X6
)

# Inspect the first 10 rows of the generated data
head(simdata, 10)

# Fit a Cox proportional hazards model to the generated data
model <- coxph(Surv(time, status) ~ X1 + X2 + X3 + X4 + X5 + X6, data = simdata)

# Display the coefficients of the fitted model
model$coefficients

write.csv(simdata, "Data_site_1.csv", row.names = FALSE)
