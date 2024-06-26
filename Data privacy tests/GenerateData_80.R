# Load required libraries
library(survival)
library(dplyr)

# Set the specific beta values
betas <- c(1, 0.5, 0.3, 0.1, 0.2, 0.05, 0.1)  # Adjust these values

# Number of observations
N <- 80

# Generate custom covariates
set.seed(128)

X1 <- rnorm(N, mean=0, sd=0.5)
X2 <- rbinom(N, size=1, prob=0.4)
X3 <- runif(N, min=0, max=0.5)
X4 <- rnorm(N, mean=0, sd=0.5)
X5 <- rbinom(N, size=1, prob=0.4)
X6 <- runif(N, min=0, max=0.5)  
X7 <- rbinom(N, size=1, prob=0.4)

# Combine into a data frame
covariates <- data.frame(X1, X2, X3, X4, X5, X6, X7)

# Calculate the linear predictor (eta)
eta <- betas[1] * X1 + betas[2] * X2 + betas[3] * X3 + betas[4] * X4 + betas[5] * X5 + betas[6] * X6 + betas[7] * X7

# Baseline hazard (constant for simplicity)
baseline_hazard <- 0.05  # Adjusted

# Generate survival times
survival_times <- rexp(N, rate = baseline_hazard * exp(eta)) * 3

# Transformation to shift the peak of the survival time distribution
shift_factor <- 8  # Adjust this factor to move the peak
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
  X7 = X7
)

# Inspect the first 10 rows of the generated data
head(simdata, 10)

# Fit a Cox proportional hazards model to the generated data
model <- coxph(Surv(time, status) ~ X1 + X2 + X3 + X4 + X5 + X6 + X7, data = simdata)

# Display the coefficients of the fitted model
model$coefficients

# Save the generated data to a CSV file
write.csv(simdata, "Data_site_5.csv", row.names = FALSE)
