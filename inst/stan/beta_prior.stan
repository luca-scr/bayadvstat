//
// Beta prior
//

data {
  real<lower=0> alpha;                 // beta shape1 parameter
  real<lower=0> beta;                  // beta shape2 parameter
}

generated quantities {
  real theta = beta_rng(alpha, beta);
}
