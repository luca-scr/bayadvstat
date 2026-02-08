//
// Binomial model with beta prior
//

data {
  int<lower=0> y;                      // binomial number of successes
  int<lower=1> n;                      // binomial number of trials
  real<lower=0> alpha;                 // shape1 parameter of beta prior
  real<lower=0> beta;                  // shape2 parameter of beta prior
  int<lower=1> n_pred;                 // number of trials for predictions
}                                     
                                      
parameters {                          
  real<lower=0, upper=1> theta;        // prob of success
}                                     
                                      
model {                               
  theta ~ beta(alpha, beta);           // prior
  y ~ binomial(n, theta);              // likelihood
}                                     
                                      
generated quantities {                
  int y_pred = binomial_rng(n_pred, theta);  // posterior predictive no. of successes
}
