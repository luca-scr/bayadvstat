//
// Normal model with normal prior on mu and half-normal on sigma
// with predictions from posterior predictive distribution
//

data {
  int<lower=0> n;                      // sample size
  array[n] real y;                     // observed data
  real mu_prior;                       // prior mean on mu
  real<lower=0> sigma_prior;           // prior std deviation on mu
  real<lower=0> tau;                   // prior std deviation on sigma
}                                      
                                       
parameters {                           
  real mu;                             // mu
  real<lower=0> sigma;                 // sigma constrained to be positive
}

model {
  mu ~ normal(mu_prior, sigma_prior);  // normal prior on mu
  sigma ~ normal(0, tau);              // half-normal prior on sigma
  y ~ normal(mu, sigma);               // likelihood
}

generated quantities {
  real y_pred = normal_rng(mu, sigma); // simulate from posterior predictive
}
