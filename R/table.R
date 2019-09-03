# MOMOpack for R

# Originally MOMOpack V 4.3 for Stata,
# created by Bernadette Gergonne, SSI-EpiLife for Euro MOMO.
# Ported into R by Theodore Lytras <thlytras@gmail.com>


# MOMO TABLES

# we keep $back of corrected data and 6 week before.

tableMOMO <- function(aggr) {
  tbl <- aggr[(aggr$wk > momoAttr$WEEK-momoAttr$delayCorr-6 & aggr$wk <= momoAttr$WEEK), c("wk", "YoDi", "WoDi", "nb2", "nbc", "Pnb", "excess", "zscore", "FLAG")]
  tbl$nbc[tbl$wk <= momoAttr$WEEK - momoAttr$delayCorr] <- NA
  tbl$wk <- NULL
  tbl$nbc <- round(tbl$nbc)
  tbl$Pnb <- round(tbl$Pnb)
  tbl$excess <- round(tbl$excess)
  tbl$zscore <- round(tbl$zscore, 2)

  tbl <- tbl[order(tbl$YoDi, tbl$WoDi),]

  tbl <- cbind(unique(aggr$Version), momoAttr$group, names(tbl), t(tbl))
  colnames(tbl) <- c("Version", "Group", "_varname", paste("v", 1:(ncol(tbl)-3), sep=""))

  tbl[,3] <- c("Year of death", "Week of death", "Number of deaths registered",
	"Corrected number of deaths", "Expected number of death (baseline)",
	"Crude variation around the baseline", "zscore", "Cusum > threshold")
  as.data.frame(tbl)

}

