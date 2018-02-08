#######################################
## author: Rob Williams              ##
## contact: jayrobwilliams@gmail.com ##
## project: misc R functions         ##
## created: February 7, 2018         ##
## updated: February 7, 2018         ##
#######################################

## this function uses ggplot2 to create a marginal effects plot from MCMC output

## depends on ggplot2 and whichever MCMC package the model object requires

## arguments:
## mod: am MCMC model object
## main: a character with the name of the variable of interest
## int: a character with the name of the interaction term
## moderator: a vector of all values for the moderating variable included in the
##            model
## point_est: a character indicating whether to use the mean or median for point
##            estimates in the table.
## seq: a numeric with the number of values to use to generate the marginal
##      effects plot
## plot: logical indicating whether to return the plot or the underlying dataframe

mcmcme <- function(mod, main, int, moderator, point_est = 'mean', seq = 100,
                   ci_level = .95, plot = T) {
  
  ## coefficients for independent and interactive effect
  samps <- extract(groups_int_fig, pars = c(main, int))
  
  ## expand moderating variable to range of values
  mod_range <- seq(min(moderator), max(moderator), length.out = seq)
  
  ## compute marginal effect for each sample
  marg <- rep(samps[[1]], seq) + samps[[2]] %o% mod_range
  
  ## calculate marginal effect for mean
  marg_mean <- apply(marg, 2, mean)
  
  ## calculate marginal effect for median and ci
  marg_med <- t(apply(marg, 2, quantile, probs = c(.5 - ci_level/2, .5,
                                                   .5 + ci_level/2)))
  
  ## create dataframe for plotting
  marg_gg <- data.frame(mod = mod_range, mean = marg_mean, median = marg_med[, 2],
                        lo = marg_med[, 1], hi = marg_med[, 3])
  
  ## use mean for point estimate
  if (point_est == 'mean') {
    
    mep <- ggplot(data = marg_gg, aes(x = mod_range, y = mean, ymin = lo, ymax = hi)) +
      geom_ribbon(alpha = .25) +
      geom_hline(yintercept = 0, lty = 2, color = 'gray40', lwd = .5) +
      geom_line()
    
  }
  
  ## use median for point esimate
  if (point_est == 'median') {
    
    mep <- ggplot(data = marg_gg, aes(x = mod_range, y = median, ymin = lo, ymax = hi)) +
      geom_ribbon(alpha = .25) +
      geom_hline(yintercept = 0, lty = 2, color = 'gray40', lwd = .5) +
      geom_line()
    
  }
  
  ## return plot
  if (plot == T) mep else marg_gg
  
}
