/*

  author: Rob Williams
  contact: jayrobwilliams@gmail.com
  project: misc Stan code
  created: October 3, 2017
  updated: November 27, 2017
  
*/

/* multinomial logit model */

data {
  
  /* dimensionality */
  int<lower=1> N; // number of observations
  int<lower=1> D; // number of predictors (p + 1)
  int<lower=2> K; // number of alternatives
  
  /* response and explanatory variables */
  int<lower=1,upper=K> Y[N]; // response variable
  vector[D] X[N]; // design matrix

}

parameters {
  
  /* hyperpriors for beta */
  real mu_beta;
  real<lower=.001> sigma_beta;
  
  /* regression parameters */
  matrix[K-1,D] beta_free; // regression coefficients for K - 1 alternatives
  
}

transformed parameters {
  
  matrix[K,D] beta; // full coefficient matrix with omitted category
  beta = append_row(rep_row_vector(0, D), beta_free); // append row of 0s to top
  
}

model {
  
  /* hyperpriors */
  mu_beta ~ normal(0, 5);
  sigma_beta ~ cauchy(0, 5);
  to_vector(beta_free) ~ normal(mu_beta, sigma_beta);
  
  /* model */
  for (n in 1:N) {
    
    Y[n] ~ categorical_logit(beta * X[n]);
  
  }
  
}
