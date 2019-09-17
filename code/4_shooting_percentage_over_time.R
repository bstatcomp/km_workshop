## Example 4: Estimating time-varying proportion

# libraries
library(ggplot2)
library(rstan)
library(mcmcse)

## Preparation -----------------------------------------------------------------
# load the data
data <- read.csv("./data/basketball_shots.csv", sep=";")
summary(data)

model <- stan_model("./Stan_solutions/bernoulli_linear_regression.stan")


## Estimating shot percentage over time for a player ---------------------------

# prepare data
y <- data[data$PlayerID == 1 & data$SpecialRim == 0,]$Made
stan_data <- list(y = y, n = length(y))

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
slope <- c(extract(samples, permuted = F)[,,2])
hist(slope)


## Estimating shot percetnage over time for all players ------------------------
getSamples <- function(y) {
  stan_data <- list(y = y, n = length(y))
  samples <- sampling(model, data = stan_data,
                      chains = 1, iter = 1200, warmup = 200, seed = 0) 
  c(extract(samples, permuted = F)[,,2])
}

res <- NULL
for (id in unique(data$PlayerID)) {
  mu0 <- getSamples(data[data$PlayerID == id & data$SpecialRim == 0,]$Made)
  res <- rbind(res, data.frame(PlayerID = id,
                               slope = mean(mu0),
                               SD_slope = sd(mu0),
                               P = mean(mu0 > 0))) # probability that diff is more than 5 deg
  
}