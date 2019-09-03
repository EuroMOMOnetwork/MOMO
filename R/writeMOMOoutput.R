# MOMOpack for R

# Originally MOMOpack V 4.3 for Stata,
# created by Bernadette Gergonne, SSI-EpiLife for Euro MOMO.
# Ported into R by Theodore Lytras <thlytras@gmail.com>


# WE CREATE THE DIRECTORIES NEEDED TO STORE THE OUTFILES
# ACCORDING TO THE WEEK UNDER STUDY


createMOMOdirectories <- function(output, wdir) {
  VERSION <- unique(output[[1]]$finalDataset$Version)

  # GLOBAL: WEEK NUMBER to STUDY according to the date of aggregation
  # = complete ISO week, (From monday to Sunday) PRECEDING the date of aggregation
  # for week from Monday to Sunday and aggregation from Monday the week after.
  WOSI <- momoAttr$WOSI
  YOSI <- momoAttr$YOSI

  country <- momoAttr$country

  outputDir <- sprintf("%s/MOMO%s-%s-%s-%s", wdir, VERSION, country, YOSI, WOSI)
  dirs <- list(
      OUTPUT = outputDir,
      CONTROL = sprintf("%s/CONTROL-MOMO-%s-%s-%s", outputDir, country, YOSI, WOSI),
      FINAL = sprintf("%s/FINAL-MOMO-%s-%s-%s", outputDir, country, YOSI, WOSI),
      COMPLETE = sprintf("%s/EUROMOMO-COMPLETE-%s-%s-%s", outputDir, country, YOSI, WOSI),
      RESTRICTED = sprintf("%s/EUROMOMO-RESTRICTED-%s-%s-%s", outputDir, country, YOSI, WOSI),
      CUMULATIVE = sprintf("%s/CUMULATIVE-MOMO-%s-%s-%s", outputDir, country, YOSI, WOSI),
      FIT = sprintf("%s/FIT-MOMO-%s-%s-%s", outputDir, country, YOSI, WOSI)
  )

  suppressWarnings(lapply(dirs, dir.create))   # Suppress the warnings if directories already exist

  return(dirs)
}



joinMOMOoutput <- function(output) {
  VERSION <- momoAttr$version
  WOSI <- momoAttr$WOSI
  YOSI <- momoAttr$YOSI
  country <- momoAttr$country

  joinedOutput <- list(
	EUROMOMOcomplete = do.call(rbind, lapply(output, function(x)x$EUROMOMOcomplete)),
	EUROMOMOrestricted = do.call(rbind, lapply(output, function(x)x$EUROMOMOrestricted)),
	MOMOtable = do.call(rbind, lapply(output, function(x)x$MOMOtable)),
	periods = list(
		cumChoice = do.call(rbind, lapply(output, function(x)x$periods$cumChoice)),
		cumWinter = do.call(rbind, lapply(output, function(x)x$periods$cumWinter)),
		cumSummer = do.call(rbind, lapply(output, function(x)x$periods$cumSummer)),
		cumYear = do.call(rbind, lapply(output, function(x)x$periods$cumYear)),
		cumSeason = do.call(rbind, lapply(output, function(x)x$periods$cumSeason))
	)
  )
  attr(joinedOutput, "VERSION") <- VERSION
  attr(joinedOutput, "WOSI") <- WOSI
  attr(joinedOutput, "YOSI") <- YOSI
  attr(joinedOutput, "country") <- country
  attr(joinedOutput$periods, "WStart") <- momoAttr$WStart
  attr(joinedOutput$periods, "WEnd") <- momoAttr$WEnd
  return(joinedOutput)
}


#' @import utils
#' @import foreign
writeMOMOoutput <- function(joinedOutput, dirs, output=NULL, emulateStata=TRUE) {

  trimDigits <- function(x) {
    for (i in 1:ncol(x)) {
        if (class(x[,i])=="numeric") {
            x[,i] <- signif(x[,i], 7)
        }
    }
    x
  }

  write.table(
    (if (!emulateStata) joinedOutput$EUROMOMOcomplete else trimDigits(joinedOutput$EUROMOMOcomplete)),
    row.names=FALSE, sep=",", quote=FALSE, na="",
    file=sprintf("%s/EUROMOMO%s-COMPLETE-%s-%s-%s.txt", dirs$COMPLETE, attr(joinedOutput, "VERSION"), attr(joinedOutput, "country"), attr(joinedOutput, "YOSI"), attr(joinedOutput, "WOSI")))
  write.table(
    (if (!emulateStata) joinedOutput$EUROMOMOrestricted else trimDigits(joinedOutput$EUROMOMOrestricted)),
    row.names=FALSE, sep=",", quote=FALSE, na="",
    file=sprintf("%s/EUROMOMO%s-RESTRICTED-%s-%s-%s.txt", dirs$RESTRICTED, attr(joinedOutput, "VERSION"), attr(joinedOutput, "country"), attr(joinedOutput, "YOSI"), attr(joinedOutput, "WOSI")))

  write.dta(joinedOutput$MOMOtable,
    sprintf("%s/TABLE-MOMO%s-%s-%s-%s.dta", dirs$FINAL, attr(joinedOutput, "VERSION"), attr(joinedOutput, "country"), attr(joinedOutput, "YOSI"), attr(joinedOutput, "WOSI")))
  write.table(joinedOutput$MOMOtable, row.names=FALSE, sep="\t", na="",
    file=sprintf("%s/TABLE-MOMO%s-%s-%s-%s.txt", dirs$FINAL, attr(joinedOutput, "VERSION"), attr(joinedOutput, "country"), attr(joinedOutput, "YOSI"), attr(joinedOutput, "WOSI")))

  write.dta(joinedOutput$periods$cumChoice,
    sprintf("%s/CUMULATIVE-Choice-w%s-w%s-MOMO-%s-%s-%s.dta", dirs$CUMULATIVE, attr(joinedOutput$periods, "WStart"), attr(joinedOutput$periods, "WEnd"), attr(joinedOutput, "country"), attr(joinedOutput, "YOSI"), attr(joinedOutput, "WOSI")))
  write.table(joinedOutput$periods$cumChoice, row.names=FALSE, sep="\t", na="",
    file=sprintf("%s/CUMULATIVE-Choice-w%s-w%s-MOMO-%s-%s-%s.txt", dirs$CUMULATIVE, attr(joinedOutput$periods, "WStart"), attr(joinedOutput$periods, "WEnd"), attr(joinedOutput, "country"), attr(joinedOutput, "YOSI"), attr(joinedOutput, "WOSI")))

  write.dta(joinedOutput$periods$cumWinter,
    sprintf("%s/CUMULATIVE-WINTER-w40-w20-MOMO-%s-%s-%s.dta", dirs$CUMULATIVE, attr(joinedOutput, "country"), attr(joinedOutput, "YOSI"), attr(joinedOutput, "WOSI")))
  write.table(joinedOutput$periods$cumWinter, row.names=FALSE, sep="\t", na="",
    file=sprintf("%s/CUMULATIVE-WINTER-w40-w20-MOMO-%s-%s-%s.txt", dirs$CUMULATIVE, attr(joinedOutput, "country"), attr(joinedOutput, "YOSI"), attr(joinedOutput, "WOSI")))

  write.dta(joinedOutput$periods$cumSummer,
    sprintf("%s/CUMULATIVE-SUMMER-w21-w39-MOMO-%s-%s-%s.dta", dirs$CUMULATIVE, attr(joinedOutput, "country"), attr(joinedOutput, "YOSI"), attr(joinedOutput, "WOSI")))
  write.table(joinedOutput$periods$cumSummer, row.names=FALSE, sep="\t", na="",
    file=sprintf("%s/CUMULATIVE-SUMMER-w21-w39-MOMO-%s-%s-%s.txt", dirs$CUMULATIVE, attr(joinedOutput, "country"), attr(joinedOutput, "YOSI"), attr(joinedOutput, "WOSI")))

  write.dta(joinedOutput$periods$cumYear,
    sprintf("%s/CUMULATIVE-YEAR-w1-w53-MOMO-%s-%s-%s.dta", dirs$CUMULATIVE, attr(joinedOutput, "country"), attr(joinedOutput, "YOSI"), attr(joinedOutput, "WOSI")))
  write.table(joinedOutput$periods$cumYear, row.names=FALSE, sep="\t", na="",
    file=sprintf("%s/CUMULATIVE-YEAR-w1-w53-MOMO-%s-%s-%s.txt", dirs$CUMULATIVE, attr(joinedOutput, "country"), attr(joinedOutput, "YOSI"), attr(joinedOutput, "WOSI")))

  write.dta(joinedOutput$periods$cumSeason,
    sprintf("%s/CUMULATIVE-SEASON-w27-w26-MOMO-%s-%s-%s.dta", dirs$CUMULATIVE, attr(joinedOutput, "country"), attr(joinedOutput, "YOSI"), attr(joinedOutput, "WOSI")))
  write.table(joinedOutput$periods$cumSeason, row.names=FALSE, sep="\t", na="",
    file=sprintf("%s/CUMULATIVE-SEASON-w27-w26-MOMO-%s-%s-%s.txt", dirs$CUMULATIVE, attr(joinedOutput, "country"), attr(joinedOutput, "YOSI"), attr(joinedOutput, "WOSI")))

  if (!is.null(output)) {
    lapply(output, function(x){
      write.dta(x$aggregate_fullDelay, sprintf("%s/delay-%s-%s-%s-%s.dta", dirs$CONTROL,
	    attr(x$aggregate, "group"), attr(joinedOutput, "country"), attr(joinedOutput, "YOSI"), attr(joinedOutput, "WOSI")))
      write.dta(x$finalDataset, sprintf("%s/DATA-MOMO%s-%s-%s-%s-%s.dta", dirs$FINAL,
	    attr(joinedOutput, "VERSION"), attr(joinedOutput, "country"), attr(x$aggregate, "group"), attr(joinedOutput, "YOSI"), attr(joinedOutput, "WOSI")))
      write.table(x$finalDataset, row.names=FALSE, sep="\t", na="", file=sprintf("%s/DATA-MOMO%s-%s-%s-%s-%s.txt", dirs$FINAL,
	    attr(joinedOutput, "VERSION"), attr(joinedOutput, "country"), attr(x$aggregate, "group"), attr(joinedOutput, "YOSI"), attr(joinedOutput, "WOSI")))
    })
  }
}

