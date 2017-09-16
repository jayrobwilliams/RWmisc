#######################################
## author: Rob Williams              ##
## contact: jayrobwilliams@gmail.com ##
## project: misc R functions         ##
## created: December 13, 2016        ##
## updated: September 16, 2017       ##
#######################################

## This script converts the TIES data, which are sanction level, to a yearly
## time-series. In doing so, it collapses states subject to multiple sanctions
## in a single year. As such, it is suited to analyses that measure whether
## states are targeted by sanctions, not ones concerned with bilateral interaction
## between targets and senders. It produces an output with target states, years,
## a dummy indicating that a sanction was threatened or imposed, a count of
## sanctions the state was subject to in that year, a dummy indicating whether
## at least one of the sanction(s) was imposed through an international
## institution, and a dummy indicating whether the sanction was actually imposed
## or not. It codes the end of any sanction without an end date as one year past
## its ongoing as of date, and drops any sanction without an ongoing as of date.



## check for duplicates and code all occurences as true, not just subsequent ones
dup.bidirec <- function (x) {
  
  ## check for duplicates from beginning and end
  duplicated(x) | duplicated(x, fromLast = T)
  
}

## read in TIES data; these are the corrected data created by running the file
## TIESCorrections.do, and then exported to .csv. you could also use foreign() to
## load the .dta file instead. be sure to put the correct file path to the data.
TIES <- read.csv('TIES.csv')

## recode ongoing sanctions as concluded one year after ongoing mention
for (i in 1:nrow(TIES)) {
  
  ## check to see that sanction i is listed as ongoing
  if (!is.na(TIES$ongoingasofyear[i])) {
    
    ## check to see if sanction i's ongoing date is less than most recent data
    if (TIES$ongoingasofyear[i] < max(TIES$ongoingasofyear, na.rm = T)) {
      
      ## code sanction i end year as one year after last mention
      TIES$endyear[i] <- TIES$ongoingasofyear[i] + 1
      
    } else {
      
      ## code sanction i end year as last recent mention
      TIES$endyear[i] <- TIES$ongoingasofyear[i]
      
    }
    
  }
  
}

## drop sanctions with no end or ongoing date
TIES <- TIES[which(!is.na(TIES$endyear)), ]

## drop ongoingasofyear variable
TIES[, 'ongoingasofyear'] <- NULL

## create object to code whether state i is subject to a sanction in year t
TIES_year <- data.frame()

## convert TIES data from sanction to sanction-year
for (i in 1:nrow(TIES)) {
  
  ## calculate date range for sanction i
  date_range <- TIES$endyear[i] - TIES$startyear[i] + 1
  
  ## create sequence for date range
  date_seq <- seq(TIES$startyear[i], TIES$endyear[i])
  
  ## extract row i into a dataframe
  temp <- TIES[i, ]
  
  ## repeat row i for each year of the sanction
  temp <- temp[rep(seq_len(nrow(temp)), date_range), ]
  
  ## create year variable for each year in date range
  temp$year <- date_seq
  
  ## append to output
  TIES_year <- rbind(TIES_year, temp)
  
}

## drop start and end year
TIES_year[, c('startyear', 'endyear')] <- list(NULL)

## code dummy for sanction present
TIES_year$sanction <- 1

## sort by year and target state before collapsing and recoding to one observation per state-year
TIES_year <- TIES_year[order(TIES_year$year, TIES_year$targetstate), ]

## recode institution NAs to 0s, may replace with multiple imputation later
TIES_year[which(is.na(TIES_year$institution)), 'institution'] <- 0

## list of each year in data
sanction_years <- unique(TIES_year$year)

## create object to hold output
TIES_ts <- data.frame()

## collapse to one observation per state-year, coding count of sanctions, allied sanction, non-allied sanction
for (i in 1:length(sanction_years)) {
  
  ## create temporary object for sanction targets in year i
  temp <- TIES_year[which(TIES_year$year == sanction_years[i]), ]
  
  ## check to see if any states are targeted by more than one sanction
  if (any(duplicated(temp$targetstate))) {
    
    ## create object of states with more than one sanction
    temp_dup <- temp[which(dup.bidirec(temp$targetstate)), ]
    
    ## list of each duplicated target in year i
    sanction_targets_dup <- unique(temp_dup$targetstate)
    
    ## create object to hold data for duplicated sanctions in year i
    output_dup <- data.frame()
    
    ## collapse each duplicated target into one entry and code sanctions count
    for (j in 1:length(sanction_targets_dup)) {
      
      temp_target <- temp_dup[which(temp_dup$targetstate == sanction_targets_dup[j]), ]
      
      ## extract year, target, and sanction from first row of temporary object (same in all rows)
      temp_output <- temp_target[1 , c('targetstate', 'year', 'sanction')]
      
      ## code count of sanctions
      temp_output$sanc_count <- nrow(temp_target)
      
      ## check if any sanctions were imposed through an institution
      if (any(temp_target$institution == 1)) {
        
        temp_output$sanc_mul <- 1
        
      } else {
        
        temp_output$sanc_mul <- 0
        
      }
      
      ## check if any sanctions were actually imposed
      if (any(temp_target$imposition == 1)) {
        
        temp_output$imposition <- 1
        
      } else {
        
        temp_output$imposition <- 0
        
      }
      
      ## append to output
      output_dup <- rbind(output_dup, temp_output)
      
    }
    
    ## perform coding operations on non-duplicated targets
    temp_sing <- temp[which(!dup.bidirec(temp$targetstate)), ]
    
    ## create object to hold data for unduplicated sanctions in year i
    output_sing <- data.frame()
    
    ## collapse each duplicated target into one entry and code sanctions count
    for (j in 1:nrow(temp_sing)) {
      
      ## extract year, target, and sanction from temporary object for sanction j in year i
      temp_output <- temp_sing[j, c('targetstate', 'year', 'sanction')]
      
      ## code sanction count as one
      temp_output$sanc_count <- 1
      
      ## check if sanction j was imposed through an institution
      if (temp_sing$institution[j] == 1) {
        
        temp_output$sanc_mul <- 1
        
      } else {
        
        temp_output$sanc_mul <- 0
        
      }
      
      ## check if sanction j was actually imposed
      if (temp_sing$imposition[j] == 1) {
        
        temp_output$imposition <- 1
        
      } else {
        
        temp_output$imposition <- 0
        
      }
      
      ## append sanction j to object for all sanctions in year i
      output_sing <- rbind(output_sing, temp_output)
      
    }
    
    ## combine output from duplicated and single sanctions
    output <- rbind(output_dup, output_sing)
    
    ## append sanctions in year i to main dataframe
    TIES_ts <- rbind(TIES_ts, output)
    
  } else {
    
    ## create object to hold data for sanctions in year i
    output <- data.frame()
    
    ## code variables for each target in year i
    for (j in 1:nrow(temp)) {
      
      ## extract year, target, and sanction from temporary object for sanction j in year i
      temp_output <- temp[j, c('targetstate', 'year', 'sanction')]
      
      ## code sanction count as one
      temp_output$sanc_count <- 1
      
      ## check if sanction j was imposed through an institution
      if (temp$institution[j] == 1) {
        
        temp_output$sanc_mul <- 1
        
      } else {
        
        temp_output$sanc_mul <- 0
        
      }
      
      ## check if sanction j was actually imposed
      if (temp$imposition[j] == 1) {
        
        temp_output$imposition <- 1
        
      } else {
        
        temp_output$imposition <- 0
        
      }
      
      ## append sanction j to object for all sanctions in year i
      output <- rbind(output, temp_output)
      
    }
    
    ## append all sanctions in year i to main dataframe
    TIES_ts <- rbind(TIES_ts, output)
    
  }
  
}

## re-sort by year and target state b/c states w/ multiple sanctions are clustered in each year
TIES_ts <- TIES_ts[order(TIES_ts$targetstate, TIES_ts$year), ]

## remove temporary objects
rm(i, j, TIES, TIES_year, output, output_dup, output_sing, temp, temp_dup, temp_sing,
   temp_output, temp_target, date_range, date_seq, sanction_years,
   sanction_targets_dup, dup.bidirec)


###################
## End of Script ##
###################