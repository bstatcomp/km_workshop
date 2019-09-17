## Example 2: Estimating the mean and comparing two means

# libraries
library(ggplot2)
library(rstan)
library(mcmcse)

## Preparation -----------------------------------------------------------------
# load the data
data <- read.csv("./data/basketball_shots.csv", sep=";")
summary(data)

# compile the Stan model (the model can be compiled once and then reused)
model <- stan_model("./Stan_solutions/bayesian_t-test.stan")


## Estimating the shot angle for a player --------------------------------------

# prepare data
y <- data[data$PlayerID == 1 & data$SpecialRim == 0,]$Angle
stan_data <- list(y = y, n = length(y), lb = 0, ub = 90)

# perform inference
samples <- sampling(model,               # compiled model
                    data = stan_data,    # data 
                    chains = 1,          # numer of independent chains
                    iter = 1200,         # total number of MCMC iterations
                    warmup = 200,        # number of "tuning" iterations
                    seed = 0)            # RNG seed (for reproducibility)

# diagnose fit
traceplot(samples, inc_warmup=TRUE)
print(samples)

# extract parameter estimates
mu <- c(extract(samples, permuted = F)[,,1])
hist(mu)


## Estimating angle adjustment for special rim for all players -----------------
getSamples <- function(y) {
  stan_data <- list(y = y, n = length(y), lb = 0, ub = 90)
  samples <- sampling(model, data = stan_data,
                      chains = 1, iter = 1200, warmup = 200, seed = 0) 
  c(extract(samples, permuted = F)[,,1])
}

res <- NULL
for (id in unique(data$PlayerID)) {
  mu0 <- getSamples(data[data$PlayerID == id & data$SpecialRim == 0,]$Angle)
  mu1 <- getSamples(data[data$PlayerID == id & data$SpecialRim == 1,]$Angle)
  res <- rbind(res, data.frame(PlayerID = id,
                               mu_norm = mean(mu0),
                               mu_spec = mean(mu1),
                               mu_diff = mean(mu0 - mu1),
                               SD_diff = sd(mu0 - mu1),
                               P = mean(abs(mu0 - mu1) > 5))) # probability that diff is more than 5 deg
  
}