# MOMOpack for R version

# Originally MOMOpack V 4.3 for Stata, 
# created by Bernadette Gergonne, SSI-EpiLife for Euro MOMO. 
# Ported into R by Theodore Lytras <thlytras@gmail.com>


# Function to automate all the period calculations done in this script

calcPeriodMOMO <- function(aggr, WStart, WEnd, wk=WStart){
  if (WStart == WEnd) stop("WStart and WEnd cannot be identical!")
  pfr <- aggr[order(aggr$YoDi, aggr$WoDi),]
  MODEL <- unique(pfr$Model)

  # if the period is in the same year
  if (WStart < WEnd) {
    pfr$Period[pfr$WoDi >= WStart & pfr$WoDi <= WEnd] <-1
    pfr$YearStart <- pfr$YoDi
    pfr$YearEnd <- pfr$YoDi
    pfr$WeekEnd <- WEnd
    pfr$WeekStart <- WStart
    pfr2 <- aggregate(pfr[pfr$Period==1, c("nbc", "Pnb", "excess", "zscore")], 
      pfr[pfr$Period==1, "YoDi", drop=FALSE], sum, na.rm=TRUE)
    colnames(pfr2) <- c("YoDi", "sumTotal", "sumBaseline", "sumExcess", "sumZscore")
    pfr2$Duration_week <- with(pfr[pfr$Period==1 & !is.na(pfr$zscore),], tapply(zscore, YoDi, length))[as.character(pfr2$YoDi)]
    pfr <- cbind(pfr, pfr2[match(pfr$YoDi, pfr2$YoDi),])
    pfr$Zzscore <- pfr$sumZscore / sqrt(pfr$Duration_week)
    pfr[,c("sumTotal", "sumBaseline", "sumExcess")] <- round(pfr[,c("sumTotal", "sumBaseline", "sumExcess")])
    pfr$Zzscore <- round(pfr$Zzscore, 2)
    pfr <- pfr[pfr$WoDi==wk,]
  }

  # if the Period is over 2 years
  if (WStart > WEnd) {
    pfr$Period <- NA
    for (x in unique(pfr$season)) {
      pfr$Period[pfr$WoDi >= WStart & pfr$YoDi == pfr$season] <- 1
      pfr$Period[pfr$WoDi <= WEnd & pfr$YoDi == pfr$season+1] <- 1
    }
    pfr$YearStart <- pfr$YoDi - as.integer(pfr$WoDi <= WStart)
    pfr$YearEnd <- pfr$YoDi - as.integer(pfr$WoDi <= WStart) + 1
    pfr$WeekEnd[pfr$Period==1] <- WEnd
    pfr$WeekStart[pfr$Period==1] <- WStart
    pfr2 <- aggregate(pfr[pfr$Period==1, c("nbc", "Pnb", "excess", "zscore")], 
      pfr[pfr$Period==1, "season", drop=FALSE], sum, na.rm=TRUE)
    colnames(pfr2) <- c("season", "sumTotal", "sumBaseline", "sumExcess", "sumZscore")
    pfr2$Duration_week <- with(pfr[pfr$Period==1 & !is.na(pfr$zscore),], tapply(zscore, season, length))[as.character(pfr2$season)]
    pfr <- cbind(pfr, pfr2[match(pfr$season, pfr2$season),])
    pfr$Zzscore <- pfr$sumZscore / sqrt(pfr$Duration_week)
    pfr[,c("sumTotal", "sumBaseline", "sumExcess")] <- round(pfr[,c("sumTotal", "sumBaseline", "sumExcess")])
    pfr$Zzscore <- round(pfr$Zzscore, 2)
    pfr <- pfr[pfr$WoDi==1,]
  }

  pfr <- pfr[,c("YearStart", "WeekStart", "YearEnd", "WeekEnd", "Duration_week", "sumTotal", "sumBaseline", "sumExcess", "Zzscore")]
  pfr <- as.data.frame(t(pfr))
  colnames(pfr) <- paste("v", 1:ncol(pfr), sep="")
  pfr$Group <- unique(aggr$GROUP)
  pfr$Version <- unique(aggr$Version)
  pfr$Model <- MODEL
  pfr$Indicator <- c("Year of Period Start", "Week of Period Start", "Year of Period End", "Week of Period End", 
	  "Duration of the Study Period", "Total number of deaths", "Expected number of death (baseline)", 
	  "Crude variation around the baseline", "Standardised variation around the baseline (Zscore)")
  pfr <- pfr[,c("Version", "Group", "Model", "Indicator", paste("v", 1:(ncol(pfr)-4), sep=""))]

  return(pfr)
}

