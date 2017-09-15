/*

  author: Rob Williams
  contact: jayrobwilliams@gmail.com
  project: misc Stan code
  created: December 30, 2016
  updated: September 14, 2017
  
*/

/* logit model */

data {
  
  /* dimensionality of data and model */
  int<lower=0> N; // number of observations
  int<lower=0> K; // number of predictors (p + 1)
  
  /* response and explanatory variables */
  int<lower=0,upper=1> Y[N]; // response variable
  matrix[N,K] X; // design matrix

}

parameters {
  
  /* parameters to be estimated by model */
  vector[K] beta; // regression coefficeients
  
}

transformed parameters {
  
  vector[N] eta; // linear predictor
  
  eta = X * beta;
  
}

model {
  
  /* priors */
  beta ~ normal(0, 2); // prior on coefficients
  
  /* response variable */
  Y ~ bernoulli_logit(eta); 
  
}
