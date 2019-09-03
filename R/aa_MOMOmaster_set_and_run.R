#' Set all the options for MoMo.
#'
#' It is usual to run \code{\link{RunMoMo}} after these options have been set.
#' @param DoA Date of aggregation (see specifications, the only information to change weekly). Provided in ISO format, i.e. YYYY-MM-DD.
#' @param DoPR Date of start of a regular MOMO registration (see specifications). Provided in ISO format, i.e. YYYY-MM-DD.
#' @param WStart Week of cumulative excess start. e.g. "influenza season" as define by EISS will be defined as WStart = 40, WEnd= 20. e.g. summer could be defined as WStart = 26, WEnd= 40
#' @param WEnd Week of cumulative excess end e.g. "influenza season" as define by EISS will be defined as WStart = 40, WEnd= 20.
#' @param country Country name
#' @param source Source of data
#' @param MFILE Name of your mortality file, either stata format (.dta) or text file (.txt) [you can use MDATA instead]
#' @param HFILE Name of file containing bank Holidays (see specifications), either stata format (.dta) or text file (.txt) [you can use HDATA instead]
#' @param INPUTDIR Input directory (where MFILE and HFILE can be found)
#' @param MDATA A data.frame containing the data that would have been contained inside MFILE. MDATA takes priority over MFILE if both are specified.
#' @param HDATA A data.frame containing the data that would have been contained inside HFILE. HDATA takes priority over HFILE if both are specified.
#' @param WDIR Output directory (all output will go here)
#' @param back The number of weeks to remove for modeling delay = the part of the series that require delay correction (see specifications).
#' @param WWW Length of retrospective historical study period in weeks
#' @param Ysum START OF CUSUM CHART: Week for CUSUM to be set to 0
#' @param Wsum START OF CUSUM CHART: Week for CUSUM to be set to 0
#' @param USEglm2 Use glm2() in order to improve convergence properties
#' @param useAUTOMN Keep using the column name "Automn" (instead of "Autumn") as in Stata MOMOpack
#' @param datesISO When saving dates in text files, use ISO format (standard in R) instead of the Stata "\%d" format
#' @param plotGraphs Setting this to FALSE suppressess the plotting of the various graphs (and saves time)
#' @param removeDataAfterDoA Set this to FALSE to include people who were registered/died after DoA
#' @param delayVersion Using the "original" delay code or the "2017-12" delay code
#' @param delayFunction This can be used instead of delayVersion. It lets you use a custom delay correction function.
#' @param delayVariance If this is true, it means that the prediction variance (from delay correction) is included in the z-scores+confidence interval
#' @param MOMOgroups Definition of the groups to be analyzed
#' @param MOMOmodels Names in the following vector should correspond to the MOMOgroups and the corresponding values (model to use for each group) should be one of "LINE", "SPLINE", "LINE_SIN", "SPLINE_SIN"
#' @param verbose Printing out information
#' @export SetOpts
SetOpts <- function(
  DoA=as.Date("2015-8-10"),
  DoPR=as.Date("2008-1-1"),
  WStart=1,
  WEnd=52,
  country = "Greece",
  source = "UoP",
  MFILE = "greece_m.dta",
  HFILE = "holidayfilegreece.dta",
  INPUTDIR = "./input",
  MDATA = NULL,
  HDATA = NULL,
  WDIR = "./output",
  back = 3,
  WWW = 290,
  Ysum = 2009,
  Wsum = 34,
  USEglm2 = TRUE,
  useAUTOMN = TRUE,
  datesISO = FALSE,
  plotGraphs = TRUE,
  removeDataAfterDoA = TRUE,
  delayVersion = "original",
  delayFunction = NULL,
  delayVariance = FALSE,
  MOMOgroups = list(
    "0to4" =  "age >= 0 & age <=4",
    "5to14" = "age >= 5 & age <=14",
    "15to64" = "age >= 15 & age <=64",
    "65P" = "age >= 65 | is.na(age)",
    "Total" = "age >= 0 | is.na(age)"
  ),
  MOMOmodels = c(
    "0to4" = "LINE",
    "5to14" = "LINE",
    "15to64" = "LINE_SIN",
    "65P" = "LINE_SIN",
    "Total" = "LINE_SIN"
  ),
  verbose=TRUE){

  if(!delayVersion %in% opts$delayVersionAvailable) stop("delayVersion not 'original' or '2017-12'")

  opts$setByUser <- TRUE

  opts$DoA <- DoA
  opts$DoPR <- DoPR
  opts$WStart <- WStart
  opts$WEnd <- WEnd
  opts$country <- country
  opts$source <- source
  opts$MFILE <- MFILE
  opts$HFILE <- HFILE
  opts$INPUTDIR <- INPUTDIR
  opts$MDATA <- MDATA
  opts$HDATA <- HDATA
  opts$WDIR <- WDIR
  opts$back <- back
  opts$WWW <- WWW
  opts$Ysum <- Ysum
  opts$Wsum <- Wsum
  opts$USEglm2 <- USEglm2
  opts$useAUTOMN <- useAUTOMN
  opts$datesISO <- datesISO
  opts$plotGraphs <- plotGraphs
  opts$removeDataAfterDoA <- removeDataAfterDoA
  opts$delayVersion <- delayVersion
  opts$delayFunction <- delayFunction
  opts$delayVariance <- delayVariance
  opts$MOMOgroups <- MOMOgroups
  opts$MOMOmodels <- MOMOmodels
  opts$verbose <- verbose
}

#' Runs the MoMo code.
#'
#' The master start button for running all of the MoMo code. \code{\link{SetOpts}} must be run first.
#' @import foreign
#' @import data.table
#' @import stringr
#' @export RunMoMo
#' @examples
#' SetOpts(
#'   DoA=as.Date("2013-12-31"),
#'   DoPR=as.Date("2008-1-1"),
#'   WStart=1,
#'   WEnd=52,
#'   country = "Denmark",
#'   source = "SSI",
#'   MFILE = "DoD_DoR.txt",
#'   HFILE = "holidays.txt",
#'   INPUTDIR = system.file("testdata",package="MOMO"),
#'   WDIR = tempdir(),
#'   back = 3,
#'   WWW = 290,
#'   Ysum = 2013,
#'   Wsum = 40,
#'   USEglm2 = TRUE,
#'   useAUTOMN = TRUE,
#'   datesISO = FALSE,
#'   plotGraphs = FALSE
#' )
#'
#' #RunMoMo()
RunMoMo <- function(){

  if(!opts$setByUser){
    stop("You have not set the options in function momo::SetOpts. You need to do this before using momo::RunMoMo")
  }
  # Analysis
  # National level
  # by population subgroup

  if(opts$verbose) cat("Welcome to MOMOpack for R.\n\n")

    # Loading in the data
    if(!is.null(opts$MDATA) & !is.null(opts$HDATA)){
      if(opts$verbose) cat("Using MDATA and HDATA as data sources\n")

      t1 <- system.time({
        MOMOfile <- opts$MDATA
        hfile <- opts$HDATA
      })

    } else {
      # Read in the input files in Stata format
      # (This section can be modified appropriately if input file is in another format)
      if(opts$verbose) cat("Reading in input files... ")
      t1 <- system.time({
        if(stringr::str_detect(opts$MFILE,".dta$")){
          MOMOfile <- read.dta(paste(opts$INPUTDIR, opts$MFILE, sep="/"))
        } else if(stringr::str_detect(opts$MFILE,".txt$")){
          MOMOfile <- as.data.frame(data.table::fread(paste(opts$INPUTDIR, opts$MFILE, sep="/")))
        } else {
          stop("unknown file type for MFILE")
        }
        #MOMOfile <- MOMOfile[,c("DoD", "DoR", "age")]


        if(stringr::str_detect(opts$HFILE,".dta$")){
          hfile <- read.dta(paste(opts$INPUTDIR, opts$HFILE, sep="/"))[,c("date", "closed")]
        } else if(stringr::str_detect(opts$HFILE,".txt$")){
          hfile <- as.data.frame(data.table::fread(paste(opts$INPUTDIR, opts$HFILE, sep="/"))[,c("date", "closed")])
        } else {
          stop("unknown file type for HFILE")
        }
      })
    }

    # A little bit of processing of the data
    MOMOfile$DoD <- as.Date(MOMOfile$DoD, origin="1960-1-1")
    MOMOfile$DoR <- as.Date(MOMOfile$DoR, origin="1960-1-1")
    if(opts$removeDataAfterDoA) MOMOfile <- MOMOfile[MOMOfile$DoD<=opts$DoA & MOMOfile$DoR<=opts$DoA,]
    hfile$date <- as.Date(hfile$date)

    if(opts$verbose) cat(sprintf("DONE (in %s seconds)\n", round(t1[3], 2)))


    if(opts$verbose) cat("\nCreating MOMO input... ")
    t2 <- system.time({
      MOMOinput <- makeMOMOinput(MOMOfile, opts$DoA, opts$DoPR, hfile,
        country=opts$country, source=opts$source, colnames=c("DoD", "DoR", "age"),
        WStart=opts$WStart, WEnd=opts$WEnd, Ysum=opts$Ysum, Wsum=opts$Wsum,
        groups=opts$MOMOgroups, models=opts$MOMOmodels, delayCorr=opts$back, histPer=opts$WWW,
        compatibility.mode=TRUE)
    })
    if(opts$verbose) cat(sprintf("DONE (in %s seconds)\n", round(t2[3], 2)))

    if(opts$verbose) cat("Iterating over age groups:\n")
    MOMOoutput <- analyzeMOMO(mi=MOMOinput, datesISO=opts$datesISO, useAUTOMN=opts$useAUTOMN,
    	USEglm2=opts$USEglm2, compatibility.mode=(opts$delayVersion=="original"), verbose=opts$verbose)

    dataExport$toSave <- vector("list",length=length(MOMOoutput))
    for(j in 1:length(dataExport$toSave)){
      dataExport$toSave[[j]] <- MOMOoutput[[j]]$toSave
      MOMOoutput[[j]]$toSave <- NULL
    }
    if(opts$verbose) cat("Joining output... ")
    MOMOjoinedOutput <- joinMOMOoutput(MOMOoutput)
    if(opts$verbose) cat("DONE\n")

    if(opts$verbose) cat("Creating MOMO directories and writing all output to disk... ")
    MOMOdirs <- createMOMOdirectories(MOMOoutput, opts$WDIR)
    writeMOMOoutput(MOMOjoinedOutput, MOMOdirs, MOMOoutput)
    if(opts$verbose) cat("DONE\n")


    if (opts$plotGraphs) {
      t3 <- system.time({
        if(opts$verbose) cat("\nPlotting graphs:")
        if(opts$verbose) cat(" (Control graphs)");
        controlGraphsMOMO(MOMOoutput, MOMOdirs)
        if(opts$verbose) cat(" (Excess graphs)");
        excessGraphsMOMO(MOMOoutput, MOMOdirs)
        if(opts$verbose) cat(" (Fit graphs)");
        fitGraphsMOMO(MOMOoutput, MOMOdirs)
        if(opts$verbose) cat(" (CUSUM graphs)");
        CUSUMgraphsMOMO(MOMOoutput, MOMOdirs)
      })
      if(opts$verbose) cat(sprintf(" DONE \n\t(in %s seconds)\n", round(t3[3], 1)))
    }


}
