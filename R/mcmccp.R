#######################################
## author: Rob Williams              ##
## contact: jayrobwilliams@gmail.com ##
## project: misc R functions         ##
## created: February 20, 2018        ##
## updated: February 20, 2018        ##
#######################################

## this function uses ggplot2 to create a coefficient plot from MCMC output

## depends on ggplot2 and whichever MCMC package the model object requires

## arguments:
## mod: am MCMC model object
## pars: a character with the name of the parameter of interest
## ci_level: a numeric indicating the credible interval to use
## plot: logical indicating whether to return the plot or the underlying dataframe

mcmccp <- function(mod, pars, ci_level, plot = T) {
  
  require(ggplot2)
  
  ## extract summary from model object
  coefs <- data.frame(rstan::summary(mod, pars = pars,
                                     probs = c(.5 - ci_level/2, .5,
                                               .5 + ci_level/2))$summary[, c(1, 4:6)])
  
  ## create variable of predictor name for plotting
  coefs$variable <- factor(rownames(coefs), levels = rownames(coefs))
  
  ## rename credible interval columns for dynamic referencing in ggplot
  colnames(coefs)[c(2, 4)] <- c('low', 'high')
  
  ## coefficient plot
  cp <- ggplot(coefs, aes(x = variable, y = mean, ymin = low, ymax = high)) +
    geom_hline(yintercept = 0, lty = 2) +
    geom_pointrange() +
    coord_flip()
  
  ## return plot or unerlying dataframe
  if (plot == T) cp else coefs
  
}
