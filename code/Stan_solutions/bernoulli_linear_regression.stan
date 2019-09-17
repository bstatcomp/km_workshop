data {
  int<lower=1> n;
  int y[n];
}

parameters {
  real<lower=0,upper=1> theta;
  real slope;
}

model {
  for (i in 1:n) {
    y[i] ~ bernoulli(inv_logit(slope * (i-1) + logit(theta)));
  }
}
