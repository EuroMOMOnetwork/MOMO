# MOMOpack for R version 0.2

# Originally MOMOpack V 4.3 for Stata,
# created by Bernadette Gergonne, SSI-EpiLife for Euro MOMO.
# Port to R and further development by Theodore Lytras <thlytras@gmail.com>

#' Data to be exported
#' @export dataExport
dataExport <- new.env(parent = emptyenv())

#' Placeholder list for all the following options
#' @export momoAttr
momoAttr <- new.env(parent = emptyenv())

#' Placeholder list for all the following options
#' @export opts
opts <- new.env(parent = emptyenv())

opts$delayVersionAvailable <- c("original","sincos","richard")

opts$setByUser <- FALSE

# CHANGE THE INFORMATION BETWEEN BRACKETS
# ACCORDING TO YOUR NEEDS and your working directories

# DATE OF AGGREGATION (see specifications, the only information to change weekly)
# (Provided in ISO format, i.e. YYYY-MM-DD)
opts$DoA <- as.Date("2015-8-10")

# DATE OF START of a regular MOMO registration (see specifications)
# (Provided in ISO format, i.e. YYYY-MM-DD)
opts$DoPR <- as.Date("2008-1-1")



# COMPARISON of CUMULATIVE EXCESS
# Chose the period of interest:
# Week of start and Week of end of the period to study EVERY YEAR or "SEASON"
# e.g. "influenza season" as define by EISS will be defined as WStart = 40, WEnd= 20
# e.g. summer could be defined as WStart = 26, WEnd= 40
# at the end, you will get a summary table for the period chosen.
opts$WStart <- 1
opts$WEnd <- 52

# COUNTRY NAME and SOURCE of data
opts$country = "Greece"
opts$source = "UoP"


# FILES NEEDED FOR ANALYSIS
# (Directories must exist, no \ at the end.)
# (Both relative and absolute pathnames work, but absolute are recommended.)

# name of your mortality file, stata format
opts$MFILE <- "greece_m.dta"

# name of file containing bank Holidays (see specifications)
opts$HFILE <- "holidayfilegreece.dta"

# INPUT DIRECTORY (where the above two files can be found)
opts$INPUTDIR <- "./input"

## alternate parameterization
# mortality data frame
opts$MDATA <- NULL

# bank holidays data frame
opts$HDATA <- NULL

# CODE DIRECTORY (where all the scripts -except this one- are to be found)
# this should not be neccessary
opts$CODEDIR <- "./code"

# OUTPUT DIRECTORY (all output will go here)
opts$WDIR <- "./output"


# CHOICE OF PARAMETERS FOR THE ANALYSIS

# chose the number of weeks to remove for modeling delay
# = the part of the series that require delay correction (see specifications)
opts$back <- 3

# choose length of retrospective historical study period in weeks
opts$WWW <- 290

# START OF CUSUM CHART: Week for CUSUM to be set to 0
opts$Ysum = 2009
opts$Wsum = 34


# ADDITIONAL OPTIONS

# Use glm2() if package glm2 is available, in order to improve convergence properties ?
# (This is equivalent to using irls option in Stata glm)
opts$USEglm2 <- TRUE

# Keep using the column name "Automn" (instead of "Autumn") as in Stata MOMOpack ?
opts$useAUTOMN <- TRUE

# When saving dates in text files, use ISO format (standard in R) instead of the Stata "%d" format ?
opts$datesISO <- FALSE

# Setting this to FALSE suppressess the plotting of the various graphs (and saves time)
opts$plotGraphs <- TRUE

# Delay version
# Either "original" or "2017-12"
opts$delayVersion <- "original"
opts$delay2Version <- "none"

# Delay function
# this allows you to use custom delay correction functions
opts$delayFunction <- NULL

# Do you want to use delayVariance
opts$delayVariance <- FALSE

# ******** DO NOT MODIFY BELOW THIS LINE ********

# DEFINITION OF the GROUPS to be analyzed
# other age group or any kind of group can be defined with the same method
# and can overlap if needed.

opts$MOMOgroups <- list(
  "0to4" =  "age >= 0 & age <=4",
  "5to14" = "age >= 5 & age <=14",
  "15to64" = "age >= 15 & age <=64",
  "65P" = "age >= 65 | is.na(age)",
  "Total" = "age >= 0 | is.na(age)"
)

# Names in the following vector should correspond to the groups above,
# and the corresponding values (model to use for each group)
# should be one of "LINE", "SPLINE", "LINE_SIN", "SPLINE_SIN"

opts$MOMOmodels <- c(
  "0to4" = "LINE",
  "5to14" = "LINE",
  "15to64" = "LINE_SIN",
  "65P" = "LINE_SIN",
  "Total" = "LINE_SIN"
)

