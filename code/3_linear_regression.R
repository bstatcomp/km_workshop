## Example 3: Linear regression

# libraries
library(ggplot2)
library(rstan)
library(mcmcse)

## Preparation -----------------------------------------------------------------
# create the data
n <- 50
x <- runif(n, -10, 10)
y <- (x - 5) * (x + 1) * (x + 5) + rnorm(n, 0, 20)
plot(x, y)

# compile the model
model <- stan_model("./Stan_solutions/linear_regression.stan")

## Linear regression -----------------------------------------------------------

# prepare data
X <- data.frame(x1 = x, x2 = 1)
stan_data <- list(y = y, n = length(y), X = X, k = 2)

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

# plot posterior distribution of lines
mycol <- rgb(0, 0, 255, max = 255, alpha = 5)
plot(x, y)
for (i in 1:1000) {
  coeff <- extract(samples)$beta[i,]
  abline(a = coeff[2], b = coeff[1], col = mycol)
}

# plot lm fit for comparison
lr <- lm(y ~ x)
abline(lr, col = "red")
