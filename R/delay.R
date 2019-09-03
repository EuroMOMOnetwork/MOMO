# MOMOpack for R

# Originally MOMOpack V 4.3 for Stata,
# created by Bernadette Gergonne, SSI-EpiLife for Euro MOMO.
# Ported into R by Theodore Lytras <thlytras@gmail.com>


# REMINDER:
# $WEEK = Week number to study according to the date of aggregation
#      = complete ISO week, (From monday to Sunday) preceding the date of aggregation
# $PRWEEK = the first week when the MOMO registration became smooth and regular
# $back = the number of week to remove from the series to model
# $WEEK2 is the week number until which we want model the series
# WRxx = the number of death registered xx week (FULL WEEK) after the week of death, ACCORDING TO WHAT WE KNOW THE DAY OF AGGREGATION
#      = number of death registered at WoS who died XX week(s) before
# WRxxA = what is registered xx week after (incomplete week) until the day of Aggregation
#       = what we know in addition if we aggregate i.e. on wednesday instead of sunday.
# YW = the ID of the week (concatenation of iso Year and iso week of death)
# WoDi = Week of Death Iso
# YoDi = Year of Death Iso
# closed = the number of day off during the iso week
# closed2 = the number of day off during the following week from Monday until the day of Aggregation
# nb = weekly number of death in the series provided
# nb2 = weekly number of death known (already registered) at the date of Aggregation
# wk = iterative number of the week
# Ywk = the stata format week number (drop week 53 !) as Stata cannot work sith ISO weeks
# nbc = corrected number of death
# nbr = registered numbre of death
# UCIc = Upper Confidence Interval of the corrected number of deaths
# LCIc = Lower Confidence Interval of the corrected number of deaths
# UPIc = Upper Prediction Interval of the corrected number of deaths
# LPIc = Lower Prediction Interval of the corrected number of deaths


delayMOMO <- function(aggr, zvalue=1.96){
  if(!is.null(opts$delayFunction)){
    cat("\nUsing custom delayFunction\n")
    opts$delayFunction(aggr=aggr, zvalue=zvalue)
  } else if(opts$delayVersion=="original"){
    delayMOMO_original(aggr=aggr, zvalue=zvalue)
  } else if(opts$delayVersion=="sincos"){
    delayMOMO_sincos(aggr=aggr, zvalue=zvalue)
  } else if(opts$delayVersion=="richard"){
    delayMOMO_richard(aggr=aggr, zvalue=zvalue)
  } else {
    stop("THIS IS NOT A VALID DELAY FUNCTION")
  }
}

trimDelayMOMO <- function(aggr) {
  predNames <- names(aggr)[stringr::str_detect(names(aggr),"^pred")]
  VpWRNames <- names(aggr)[stringr::str_detect(names(aggr),"^VpWR")]

  if(!opts$delayVariance){
    extraVars <- predNames
  } else {
    extraVars <- c(predNames,VpWRNames)
  }

  ret <- aggr[,c("GROUP", "WoDi", "YoDi", "wk", "wk2", "nb", "nb2", "nbr", "nbc", extraVars, "UCIc", "LCIc", "UPIc", "LPIc")]

  # We must preserve the attributes we need
  transferMOMOattributes(ret, aggr)
}
