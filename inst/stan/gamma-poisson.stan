//
// Poisson model with Gamma prior
//

data {
  int<lower=0> n;                      // sample size
  array[n] int<lower=0> y;             // observed counts
  real<lower=0> alpha;                 // shape parameter of the Gamma prior
  real<lower=0> beta;                  // rate parameter of the Gamma prior
}                                      
                                       
parameters {                           
  real<lower=0> lambda;                // rate parameter (Poisson)
}

model {
  lambda ~ gamma(alpha, beta);         // prior
  y ~ poisson(lambda);                 // likelihood
}
