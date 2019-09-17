data {
  int<lower=1> n;
  real y[n];
  real lb;
  real ub;
}

parameters {
  real mu;
  real<lower=0> sigma;
}

model {
  // prior
  mu ~ uniform(lb, ub);

  for (i in 1:n) {
    y[i] ~ normal(mu, sigma);
  }
}
