//
// Beta prior
//

data {
  real<lower=0> alpha;                 // shape1 parameter of beta prior
  real<lower=0> beta;                  // shape2 parameter of beta prior
}

generated quantities {
  real theta = beta_rng(alpha, beta);
}
