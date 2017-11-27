/*
  
  author: Rob Williams
  contact: jayrobwilliams@gmail.com
  project: misc Stan code
  created: November 27, 2017
  updated: November 27, 2017
  
*/

/* weibull model with random intercepts */

data {
  
  /* dimensionality */
  int<lower=1> N_obs; // number of observed observations
  int<lower=1> N_cen; // number of censored observations
  int<lower=0> K; // number of predictors
  int<lower=1> G; // number of groups
  int<lower=1> groups_obs[N_obs]; // group ID indexing vector for observed observations
  int<lower=1> groups_cen[N_cen]; // group ID indexing vector for censored observations
  
  /* outcomes */
  real<lower=0> y_obs[N_obs]; // observed outcomes
  real<lower=0> y_cen[N_cen]; // censoring times
  
  /* predictors */
  matrix[N_obs, K] X_obs; // predictors for observed observations
  matrix[N_cen, K] X_cen; // predictors for censored observations
  
}

parameters {
  
  /* hyperpriors */
  real mu_delta;
  real<lower=.001> sigma_delta;
  real<lower=.001> alpha_gamma;
  real<lower=.001> beta_gamma;
  
  /* regression parameters */
  real<lower=.001> alpha; // shape parameter
  vector[K] beta; // predictor coefficients
  vector[G] delta; // group random intercepts
  
}

model {
  
  /* hyperpriors and priors */
  beta ~ student_t(4, 0, 1); // coefficient priors
  
  alpha_gamma ~ exponential(.25); // hyperpriors on shape parameter
  beta_gamma ~ exponential(.25);
  alpha ~ gamma(alpha_gamma, beta_gamma);
  
  mu_delta ~ normal(0, 5); // hyperpriors on group random intercept
  sigma_delta ~ cauchy(0, 5);
  delta ~ normal(mu_delta, sigma_delta);
  
  /* model */
  y_obs ~ weibull(alpha, exp(-(delta[groups_obs] + X_obs * beta) / alpha));
  target += weibull_lccdf(y_cen | alpha, exp(-(delta[groups_cen] + X_cen * beta) / alpha)); 
  
}
