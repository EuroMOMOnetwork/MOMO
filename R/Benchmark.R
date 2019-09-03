#' Benchmark different delay corrections.
#' @param tag Tag for file name
#' @param wdir workingdir
#' @import data.table
#' @importFrom lubridate today
#' @export BenchmarkDelay
BenchmarkDelay <- function(tag,wdir=NULL){
  if(!opts$setByUser){
    stop("You have not set the options in function momo::SetOpts. You need to do this before using momo::RunMoMo")
  }

  # remove some of the more pointless functions for a benchmark
  if(is.null(wdir)) wdir <- opts$WDIR
  opts$WDIR <- tempdir()
  opts$verbose <- F

  RunMoMo()

  trueRes <- data.table::rbindlist(dataExport$toSave)
  predNames <- names(trueRes)[stringr::str_detect(names(trueRes),"^predzscore")]

  trueRes <- trueRes[,c("GROUP","YoDi","WoDi","excess","zscore","Pnb","UPIb2",predNames),with=F]

  res <- dataExport$aggr_fullDelay
  res <- merge(res,trueRes,by=c("GROUP","YoDi","WoDi"))
  setDT(res)

  opts$WDIR <- wdir

  modellingWeeks <- momoAttr$PRWEEK:momoAttr$WEEK2

  x <- res[wk %in% modellingWeeks]
  ra <- dataExport$raw

  rmarkdown::render(input = file.path(system.file("ext",package="MOMO"),"BenchmarkDelay.Rmd"),
                    output_file = sprintf("BenchmarkDelay_%s_%s_%s.pdf",opts$country,tag,lubridate::today()),
                    output_dir = wdir, output_format = rmarkdown::pdf_document(toc=TRUE),
                    params = list(x=x, ra=ra))

  return(x)
}

#' @import ggplot2
#' @importFrom readxl read_excel
BenchmarkNorway <- function(wdir=system.file("benchmarkdelay",package="MOMO")){
  #devtools::load_all()
  #library(data.table)
  #library(ggplot2)

  masterData <- fread("/docs/dashboards/data_raw/normomo/FHIDOD2_20180814.txt")
  masterData[,DoD:=as.Date(as.character(DODS_DATO),format="%Y%m%d")]
  masterData[,DoR:=as.Date(as.character(ENDR_DATO),format="%Y%m%d")]
  masterData[,DoB:=as.Date(as.character(FDATO_YYYYMMDD),format="%Y%m%d")]
  masterData[,age:=floor(as.numeric(difftime(DoD,DoB,units="days"))/365.25)]
  masterData[is.na(DoR),DoR:=DoD+1]
  masterData[DoR>="2015-09-03",DoR:=DoR+1]

  masterData[,ageCat:=cut(age,c(0,4,14,64,200),include.lowest = TRUE)]
  masterData[,deathWeek:=WeekN(masterData$DoD)]
  masterData[,deathYear:=YearN(masterData$DoD)]
  MDATA <- as.data.frame(masterData[!is.na(age),c("DoD","DoR","age")])

  HDATA <- data.frame(readxl::read_excel(system.file("extdata", "bank_holidays.xlsx", package = "normomo"))[,c("date", "closed")])
  HDATA$date <- as.Date(HDATA$date)

  for(dv in c("original","sincos","richard")){
  #for(dv in c("dummyyear")){
    SetOpts(
      DoA=as.Date("2018-08-14"),
      DoPR=as.Date("2012-01-01"),
      WStart=1,
      WEnd=52,
      country = "Norway",
      source = "FHI",
      MDATA = MDATA,
      HDATA = HDATA,
      INPUTDIR = tempdir(),
      WDIR = tempdir(),
      back = 7,
      WWW = 290,
      Ysum = 2018,
      Wsum = 40,
      USEglm2 = TRUE,
      useAUTOMN = TRUE,
      datesISO = FALSE,
      plotGraphs = FALSE,
      delayVersion = dv,
      MOMOgroups = list("Total"="age >= 0 | is.na(age)"),
      MOMOmodels = list("Total"="LINE_SIN"))
    #delayFunction = MOMO::delayMOMO_original,
    x <- BenchmarkDelay(tag=dv,wdir=wdir)
  }
}

BenchmarkDenmark <- function(wdir=system.file("benchmarkdelay",package="MOMO")){
  #devtools::load_all()
  #library(data.table)
  #library(ggplot2)

  MDATA <- as.data.frame(data.table::fread(file.path(system.file("testdata",package="MOMO"),"DoD_DoR.txt")))
  HDATA <- as.data.frame(data.table::fread(file.path(system.file("testdata",package="MOMO"),"holidays.txt")))

  for(dv in c("original","richard","richard1","richard2")){
    SetOpts(
      DoA=as.Date("2013-12-31"),
      DoPR=as.Date("2008-1-1"),
      WStart=1,
      WEnd=52,
      country = "Denmark",
      source = "SSI",
      MDATA = MDATA,
      HDATA = HDATA,
      INPUTDIR = tempdir(),
      WDIR = wdir,
      back = 7,
      WWW = 290,
      Ysum = 2018,
      Wsum = 40,
      USEglm2 = TRUE,
      useAUTOMN = TRUE,
      datesISO = FALSE,
      plotGraphs = FALSE,
      delayVersion = dv,
      MOMOgroups = list("Total"="age >= 0 | is.na(age)"),
      MOMOmodels = list("Total"="LINE_SIN"))

    x <- BenchmarkDelay(tag=dv)
  }
}

