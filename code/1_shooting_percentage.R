## Example 1: Estimating a binomial proportion and comparing two binomial proportions

# libraries
library(ggplot2)
library(rstan)
library(mcmcse)

## Preparation -----------------------------------------------------------------
# load the data
data <- read.csv("./data/basketball_shots.csv", sep=";")
summary(data)

# compile the Stan model (the model can be compiled once and then reused)
model <- stan_model("./Stan_solutions/bernoulli.stan")


## Estimating the shooting percentage of player 1 (normal rim) -----------------

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
theta1 <- c(extract(samples, permuted = F)[,,1])
hist(theta1)

# answering probabilisitc questions
mcse(theta1 > 0.65) # probability that player 1 hits more than 50% of shots

## Estimating the shooting percentage of player 3 (normal rim) -----------------

# prepare data
y <- data[data$PlayerID == 5 & data$SpecialRim == 0,]$Made
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
theta2 <- c(extract(samples, permuted = F)[,,1])
hist(theta2)

# answering probabilisitc questions
mcse(theta2 > 0.65) # probability that player 1 hits more than 50% of shots


## Comparing the two players ---------------------------------------------------
mcse(theta1 < theta2) # Is player 5 better than player 1?