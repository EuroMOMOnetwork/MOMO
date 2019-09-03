#' Year
#' @param date The date of interest
#' @importFrom lubridate today
#' @export Year
Year <- function(date=lubridate::today()){
  yr <- format.Date(date,"%G")
  return(yr)
}

#' YearC
#' @param date The date of interest
#' @importFrom lubridate today
#' @export YearC
YearC <- function(date=lubridate::today()){
  yr <- format.Date(date,"%G")
  return(yr)
}

#' YearN
#' @param date The date of interest
#' @importFrom lubridate today
#' @export YearN
YearN <- function(date=lubridate::today()){
  yr <- as.numeric(format.Date(date,"%G"))
  return(yr)
}

#' WeekC
#' @param date The date of interest
#' @importFrom lubridate today
#' @export WeekC
WeekC <- function(date=lubridate::today()){
  wk <- as.numeric(format.Date(date,"%V"))
  wk <- formatC(wk, flag = "0", width = 2)
  return(wk)
}

#' WeekN
#' @param date The date of interest
#' @importFrom lubridate today
#' @export WeekN
WeekN <- function(date=lubridate::today()){
  wk <- as.numeric(format.Date(date,"%V"))
  return(wk)
}

#' YearWeek
#' @param date The date of interest
#' @importFrom lubridate today
#' @export YearWeek
YearWeek <- function(date=lubridate::today()){
  return(sprintf("%s-%s",Year(date),WeekC(date)))
}

