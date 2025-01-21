//
// Normal model with normal prior on mu (and sigma known)
//

data {
  int<lower=0> n;                      // sample size
  array[n] real y;                     // observed data
  real mu_prior;                       // prior mean
  real<lower=0> sigma_prior;           // prior std deviation
  real<lower=0> sigma;                 // known value of sigma
}

parameters {
  real mu;                             // mean
}

model {
  mu ~ normal(mu_prior, sigma_prior);  // normal prior on mu
  y ~ normal(mu, sigma);               // likelihood
}
