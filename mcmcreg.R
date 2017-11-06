#######################################
## author: Rob Williams              ##
## contact: jayrobwilliams@gmail.com ##
## project: misc R functions         ##
## created: November 4, 2017         ##
## updated: November 5, 2017         ##
#######################################

## this function is a wrapper to texreg() which produces a standard regression
## table with interval measure of uncertainty for MCMC output. the user can
## specify the confidence level of the interval, as well as credible or highest
## posterior density intervals. many of the arguments are standard texreg()
## arguments that are passed to texreg() at the end of the function to create
## the table.

## depends on texreg and whichever MCMC package the model object(s) require

## arguments:
## mod: a single MCMC model object, or a list of model objects of the same class.
## pars: a scalar or vector of the parameters you wish to include in the table.
##       stanfit objects can use either the individual parameter names, or the
##       names of the indexed parameter to retrieve the entire parameter e.g.
##       pars = 'beta' will return beta[1], beta[2], and beta[3] for a stanfit
##       model with a three element beta parameter. parameter arguments for all
##       other model object types must contain the entire set of parameters you
##       wish to include in the table.
## point_est: a character indicating whether to use the mean or median for point
##            estimates in the table.
## ci: a scalar indicating the confidence level of the uncertainty intervals.
## hpdi: a logical indicating whether to use highest posterior density intervals.
## model_names: an optional vector of models names.
## custom_coef: an optional vector of parameter names. if not supplied, the
##              function will use the parameter names in the model object(s).
## caption: an optional caption for the table.
## label: an optional label for the table.

mcmcreg <- function(mod, pars, point_est = 'mean', ci = .95, hpdi = F,
                    model_names, custom_coef, caption, label, sideways = F,
                    reorder_coef, file_name) {
  
  ## if only one model object, coerce to a list
  if (class(mod) != 'list') mod <- list(mod)
  
  ## if no model names, assign defaults
  if (missing(model_names)) model_names <- NULL
  
  ## if no caption, assign default
  if (missing(caption) & length(mod) > 1) caption <- 'Statistical Models'
  if (missing(caption) & length(mod) == 1) caption <- 'Statistical Model'
  
  ## if no label, assign default
  if (missing(label)) label <- NULL
  
  ## if no custom coefficient order, assign default
  if (missing(reorder_coef)) reorder_coef <- NULL
  
  ## extract samples and variable names from stanfit object
  if (lapply(mod, class)[[1]] == 'stanfit') {
    
    ## extract coefficient names from list of model ojects
    coef_names <- lapply(mod, function(x) rownames(rstan::summary(x, pars = pars)$summary))
    
    ## extract posterior samples from list of model objects
    samps <- lapply(mod, function(x) as.data.frame(rstan::extract(x, pars = pars)))
    
  }
  
  ## extract samples and variable names from runjags object
  if (lapply(mod, class)[[1]] == 'runjags') {
    
    ## extract posterior samples from list of model objects
    samps <- lapply(mod, function(x) runjags:::as.mcmc.list.runjags(x, vars = pars))
    
    ## average over chains and convert to dataframe
    samps <- lapply(samps, function(x) as.data.frame(Reduce("+", x) / length(x)))
    
    ## extract coefficient names from dataframe
    coef_names <- lapply(samps, colnames)
    
  }
  
  ## extract samples and variable names from mcmc.list object
  if (lapply(mod, class)[[1]] == 'mcmc.list') {
    
    ## extract posterior samples from list of model objects
    samps <- lapply(mod, function(x) as.data.frame(Reduce("+", x) / length(x)))
    
    ## drop columns not in pars
    samps <- lapply(samps, function(x) x[, colnames(x) %in% pars])
    
    ## extract coefficient names from dataframe
    coef_names <- lapply(samps, colnames)
    
  }
  
  ## extract samples and variable names from mcmc object
  if (lapply(mod, class)[[1]] == 'mcmc') {
    
    ## extract posterior samples from list of model objects
    samps <- lapply(mod, function(x) coda:::as.data.frame.mcmc(x, vars = pars))
    
    ## extract coefficient names from dataframe
    coef_names <- lapply(samps, colnames)
    
  }
  
  ## calculate point estimate of posterior density
  if (point_est == 'mean') {
    
    samps_pe <- lapply(samps, function(x) apply(x, 2, mean))
    
  } else {
    
    samps_pe <- lapply(samps, function(x) apply(x, 2, median))
    
  }
  
  ## calculate uncertainty interval for ci argument
  if (hpdi == F) {
    
    samps_ci <- lapply(samps, function(x) apply(x, 2, quantile,
                                                probs = c(.5 - ci/2, .5 + ci/2)))
    
  } else {
    
    samps_ci <- lapply(samps, function(x) t(coda::HPDinterval(coda::as.mcmc(x),
                                                            prob = ci)))
    
  }
  
  ## if coefficent names supplied, replace names from model object(s)
  if (!missing(custom_coef)) coef_names <- custom_coef
  
  ## create list of texreg object(s) with point estimates and interval
  tr_list <- mapply(function(x, y, z) texreg::createTexreg(coef.names = x,
                                                           coef = y,
                                                           ci.low = z[1, ],
                                                           ci.up = z[2, ]),
                    coef_names, samps_pe, samps_ci)
  
  ## create LaTeX code
  tr <- texreg::texreg(l = tr_list, custom.model.names = model_names,
                       caption = caption, label = label, sideways = sideways,
                       reorder.coef = reorder_coef, use.packages = F)
  ## replace confidence w/ credible or highest posterior density in texreg output
  if (hpdi == F) {
    
    tr <- sub('outside the confidence interval',
              paste('outside ', ci * 100 ,'\\\\% credible interval', sep = ''),
              tr)
    
  } else {
    
    tr <- sub('outside the confidence interval',
              paste('outside ', ci * 100 ,'\\\\% highest posterior density interval',
                    sep = ''), tr)
    
  }
  
  ## return LaTeX code to console or write to file
  if (missing(file_name)) {
    
    tr
    
  } else {
    
    ## remove newline at start of LaTeX code
    tr <- sub('^\\n', '', tr)
    
    tex_file <- file(paste(file_name, 'tex', sep = '.'))
    writeLines(tr, tex_file, sep = '')
    close(tex_file)
    
  }
  
}

## need to figure out how to get log likelihood matrix from various objects for WAIC