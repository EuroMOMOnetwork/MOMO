# MOMOpack for R

# Originally MOMOpack V 4.3 for Stata,
# created by Bernadette Gergonne, SSI-EpiLife for Euro MOMO.
# Ported into R by Theodore Lytras <thlytras@gmail.com>

EUROMOMOoutput <- function(aggr, useAUTOMN=FALSE, datesISO=TRUE) {
  aggr2 <- aggr

  # Country and source identifiers
  aggr2$source <- momoAttr$source
  aggr2$country <- momoAttr$country
  names(aggr2)[names(aggr2)=="GROUP"] <- "group"

  aggr2 <- aggr2[,c("Version", "Model", "source", "country", "group", "wk", "WoDi", "YoDi",
	"wk2", "nb", "nbr", "nbc", "UCIc", "LCIc", "UPIc", "LPIc", "Pnb", "UPIb2", "LCIb",
	"UCIb", "excess", "UCIe", "LCIe", "zscore", "DOTm", "DOTc", "DOTb", "DOTz", "DOTzm",
	"Spring", c("Autumn","Automn")[useAUTOMN+1],
	"CUSUM", "FLAG", "k", "ARL0", "h")]

  # DateoA, YoAi and WoAi gives the time when the data where aggregated,
  # which should ideally be the week of output transfer to EuroMOMO
  aggr2$DateoA <- momoAttr$DoA
  aggr2$YoAi <- momoAttr$YOAI
  aggr2$WoAi <- momoAttr$WOAI

  aggr2$flag[aggr2$wk>momoAttr$WEEK2 - 7] <- 1
  aggr2$flag[aggr2$wk<momoAttr$WEEK2 - 6] <- 0

  if (!datesISO) {
    months.en <- c("jan", "feb", "mar", "apr", "may", "jun", "jul", "aug", "sep", "oct", "nov", "dec")
    aggr2$DateoA <- paste(format(aggr2$DateoA, "%d"), months.en[as.integer(format(aggr2$DateoA, "%m"))], format(aggr2$DateoA, "%Y"), sep="")
  }

  list(
    COMPLETE = aggr2[,c("Version", "Model", "source", "country", "group", "DateoA", "WoAi", "YoAi",
	"WoDi", "YoDi", "wk2", "UCIc", "LCIc", "nbr", "excess", "zscore", "DOTc", "DOTb", "DOTz",
	"Pnb", "Spring", c("Autumn","Automn")[useAUTOMN+1], "nb", "nbc", "UPIc", "LPIc", "UPIb2",
	"LCIb", "UCIb", "UCIe", "LCIe", "DOTm", "DOTzm", "CUSUM", "FLAG", "k", "ARL0", "h")],
    RESTRICTED = aggr2[,c("Version", "Model", "source", "country", "group", "DateoA", "WoAi", "YoAi",
	"WoDi", "YoDi", "wk2", "zscore", "DOTz", "Spring", c("Autumn","Automn")[useAUTOMN+1],
	"FLAG", "k", "ARL0", "h")])

}

