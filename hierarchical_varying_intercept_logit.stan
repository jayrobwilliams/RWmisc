/*

  author: Rob Williams
  contact: jayrobwilliams@gmail.com
  project: misc Stan code
  created: December 30, 2016
  updated: September 14, 2017
  
*/

/* hierarchical varying intercept logit model */

data {
  
  /* dimensionality of data and model */
  int<lower=1> N; // number of observations
  int<lower=1> K; // number of observation level predictors (p + 1)
  int<lower=1> G; // number of groups
  int<lower=1> L; // number of group level predictors
  
  /* indices */
  int<lower=1,upper=G> id[N]; // group id variable
  
  /* response and explanatory variables */
  int<lower=0,upper=1> Y[N]; // response variable
  matrix[N,K] X; // design matrix for observation level predictors
  matrix[G,L] Z; // design matrix for group level predictors
  
}

parameters {
  
  /* parameters to be estimated by model */
  vector[K] beta; // regression coefficeients, minus varying intercept
  vector[G] alpha; // varying intercept
  vector[L] gamma; // coefficients on varying intercept
  real<lower=0> sigma; // standard deviation of varying intercept
  
}

transformed parameters {
  
  vector[G] xi; // linear predictor for varying intercept
  vector[N] eta; // linear predictor for response variable
  
  xi = Z * gamma; // group level linear predictor
  eta = alpha[id] + X * beta; // add varying intercept
  
}

model {
  
  /* priors */
  gamma ~ cauchy(0, 2.5); // prior on varying intercept coefficients
  sigma ~ gamma(1,.5); // prior on standard deviation of varying intercept
  beta ~ cauchy(0, 2.5); // prior on response variable coefficients
  
  /* varying intercept */
  alpha ~ normal(xi, sigma);
  
  /* response variable */
  Y ~ bernoulli_logit(eta); 
  
}
