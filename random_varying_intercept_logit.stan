/*

  author: Rob Williams
  contact: jayrobwilliams@gmail.com
  project: misc Stan code
  created: December 30, 2016
  updated: September 14, 2017
  
*/

/* random varying intercept logit model */

data {
  
  /* dimensionality of data and model */
  int<lower=1> N; // number of observations
  int<lower=1> K; // number of predictors (p + 1)
  int<lower=1> G; // number of groups
  
  /* indices */
  int<lower=1,upper=G> id[N]; // group id variable
  
  /* response and explanatory variables */
  int<lower=0,upper=1> Y[N]; // response variable
  matrix[N,K] X; // design matrix

}

parameters {
  
  /* parameters to be estimated by model */
  vector[K] beta; // regression coefficeients, minus varying intercept
  vector[G] alpha; // varying intercept
  
}

transformed parameters {
  
  vector[N] eta; // linear predictor
  
  eta = alpha[id] + X * beta; // add varying intercept
  
}

model {
  
  /* priors */
  beta ~ normal(0, 2); // prior on coefficients
  alpha ~ normal(0, 2); // prior on varying intercept
  
  /* response variable */
  Y ~ bernoulli_logit(eta); 
  
}
