#######################################
## author: Rob Williams              ##
## contact: jayrobwilliams@gmail.com ##
## project: misc R functions         ##
## created: July 9, 2017             ##
## updated: September 13, 2017       ##
#######################################



## This script contains various functions to create a rudimentary regression
## output table for multiple imputation models using MICE.. The function
## MI.table() creates the table, while the others are subfuctions which prepare
##the necessary  information. MI.table() can produce LaTeX or html output, and
## can save the resulting table to a file if desired. MI.table() is designed to
## only be used on a single model object at a time; the user will have to manually
## combine the output from multiple models into one table if they wish to present
## results from multiple models together. MI.table() accepts either raw MIRA or
## pooled MIPO output from MICE.



## rudimentary LaTeX tables for MI models
MI.table <- function(MImodel, coef.names, model.name, cap = 'MI Models',
                     lab = 'mi_mod', pval = .05, file,
                     method = c('latex', 'kable', 'html')) {
  
  ## load required packages
  require(mice)
  require(xtable)
  require(knitr)
  
  ## check if model is MIRA or MIPO
  if (class(MImodel)[1] == 'mira') {
    
    ## if model is MIRA, pool to MIPO
    MImodel <- pool(MImodel)
    
  }
  
  ## get number of observations in complete data from original MICE object
  N <- nrow(get(as.character(MImodel$call2)[2])$data)
  
  ## if coef names not supplied, extract from model object
  if (missing(coef.names)) coef.names <- names(MImodel$qbar)
  
  ## if model name not supplied, create generic
  if (missing(model.name)) model.name <- 'Model 1'
  
  ## get coefficient and SE estimates for model
  tab <- MI.coef.SE(MImodel, pval)
  
  ## add p value cutoff to last row
  tab <- c(tab, N, '')
  
  ## rename matrix rows using coefficient names, with a blank every other row
  tab_rownames <- c(rbind(coef.names, ''), 'N', paste('$^*p < ', pval, '$', sep = ''))
  
  ## create dataframe with rownames and values
  tab <- data.frame(tab_rownames, tab)
  
  ## drop rownames column name and rename model column
  colnames(tab) <- c('', model.name)
  
  ## latex method
  if (method == 'latex') {
    
    ## check if file name supplied
    if(!missing(file)) {
      
      ## combine coefficient and SE estimates for each model into matrix; save
      print(xtable(tab, caption = cap, label = lab),
            sanitize.text.function = function(x){x}, include.rownames = F,
            hline.after = c(-1,0,nrow(tab) - 2, nrow(tab)), file = file)
      
    } else {
      
      ## combine coefficient and SE estimates for each model into matrix; print
      print(xtable(tab, caption = cap, label = lab),
            sanitize.text.function = function(x){x}, include.rownames = F,
            hline.after = c(-1,0,nrow(tab) - 2, nrow(tab)))
      
    }
    
  }
  
  ## kable method
  if (method == 'kable') {
    
    print(kable(tab, caption = cap))
    
  }
  
  ## html method
  if (method == 'html') {
    
    ## check if file name supplied
    if(!missing(file)) {
      
      ## combine coefficient and SE estimates for each model into matrix; save
      print(xtable(tab, caption = cap, label = lab),
            sanitize.text.function = function(x){x}, include.rownames = F,
            hline.after = c(-1,0,nrow(tab) - 2, nrow(tab)), type = 'html',
            file = file)
      
    } else {
      
      ## combine coefficient and SE estimates for each model into matrix; print
      print(xtable(tab, caption = cap, label = lab),
            sanitize.text.function = function(x){x}, include.rownames = F,
            hline.after = c(-1,0,nrow(tab) - 2, nrow(tab)), type = 'html')
      
    }
    
  }
  
}

## create alternating vectors of coefficients and SEs for tables
MI.coef.SE <- function(model, pval_cutoff) {
  
  ## get logical vector of stars/no stars for coefficients
  stars <- MI.pval(model, pval_cutoff)
  
  ## convert from logical vector to character vector of actual stars
  stars <- MI.star(stars)
  
  ## round coefficient and SE to 2 digits, keep trailing zeroes, and alternate
  test <- t(matrix(c(rbind(paste(sprintf('%.2f', round(model$qbar, 2)), stars, sep = ''),
                           paste('(', sprintf('%.2f', round(sqrt(diag(model$t)), 2)), ')',
                                 sep = '')))))
  
}

## calculate p-values for MI model coefficients
MI.pval <- function(MImodel, cutoff) {
  
  ## perform two-tailed t test on each variable, return logical vector
  cutoff > (pt(MImodel$qbar / sqrt(diag(MImodel$t)), df = MImodel$df,
               lower.tail = T)) |
    cutoff > (pt(MImodel$qbar / sqrt(diag(MImodel$t)), df = MImodel$df,
                 lower.tail = F))
  
}

## create character vector of stars for significant coefficients
MI.star <- function(pvals) {
  
  ## create vector to hold stars
  stars <- character()
  
  ## for each variable, add a star or not
  for (i in 1:length(pvals)) {
    
    if (pvals[i] == T) {stars[i] <- '$^*$'} else {stars[i] <- ''}
    
  }
  
  ## return character vector of stars
  stars
  
}



###################
## End of script ##
###################