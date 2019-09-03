# MOMOpack for R

# Originally MOMOpack V 4.3 for Stata,
# created by Bernadette Gergonne, SSI-EpiLife for Euro MOMO.
# Ported into R by Theodore Lytras <thlytras@gmail.com>


# Function to prepare a MOMO input file

makeMOMOinput <- function(df, DoA, DoPR, hfile, country, source,
	WStart, WEnd, Ysum, Wsum,
	colnames=c("DoD", "DoR", "age"),
	groups=NULL, models=rep("LINE_SIN", length(groups)), delayCorr=3, histPer=290,
	compatibility.mode=FALSE) {
  # Checking arguments
  if (class(df)!="data.frame")
    stop("Argument 'df' should be a data.frame.")
  if (length(source)!=1 || length(country)!=1)
    stop("Arguments 'country' and 'source' should be of length one.")
  if (length(Ysum)!=1 || length(Wsum)!=1 || !is.numeric(Ysum) || !is.numeric(Wsum))
    stop("Arguments 'Ysum' and 'Wsum' should be of length one.")
  if (is.vector(colnames) && is.numeric(colnames)) colnames <- names(df)[colnames]
  if (sum(colnames %in% names(df), na.rm=TRUE) < 3)
    stop("Did not find the provided column names in the supplied data.frame.")
  DoA <- as.Date(DoA, format="%Y-%m-%d") # We require the standard ISO format
  if (is.na(DoA))
    stop("Invalid date of aggregation supplied (please use ISO format: YYYY-mm-dd).")
  DoPR <- as.Date(DoPR, format="%Y-%m-%d") # We require the standard ISO format
  if (is.na(DoPR))
    stop("Invalid date of regular MOMO registration supplied (please use ISO format: YYYY-mm-dd).")
  if (is.null(groups)) {
    groups <- list("Total" = TRUE)
    models <- c("Total" = "LINE_SIN")
  } else {
    if (is.null(names(models))) names(models) <- names(groups)
    if (!identical(sort(names(models)), sort(names(groups))))
      stop("Argument 'models' must have the same length as argument 'groups'.")
    if (sum(!(models %in% c("LINE", "SPLINE", "LINE_SIN", "SPLINE_SIN")), na.rm=TRUE)>0)
      stop("Values in models must be one of \"LINE\", \"SPLINE\", \"LINE_SIN\", \"SPLINE_SIN\"")
  }
  names(df)[match(colnames, names(df))] <- c("DoD", "DoR", "age")
  if (!is.numeric(df$age))
    stop("In the supplied data.frame, the column for age is not numeric.")
  if (class(df$DoD)!="Date" || class(df$DoR)!="Date")
    stop(paste("In the supplied data.frame, the column for date of death",
      "and/or date of registration is not of class \"Date\".",
      "To avoid subtle errors, please explicitly convert them using as.Date().", sep="\n"))
  if (length(delayCorr)!=1 || !is.numeric(delayCorr) || is.na(delayCorr))
    stop("Argument 'delayCorr' should be the number of weeks to adjust for modelling delay.")
  if (length(histPer)!=1 || !is.numeric(histPer) || is.na(histPer))
    stop("Argument 'histPer' should be the length of retrospective historical study period in weeks.")

  dataExport$raw <- copy(df)

  for (g in names(groups)) {
    if (is.na(groups[[g]])) {
      names(df)[names(df)==g] <- sprintf("GRP%s", g)
    } else {
      df[,sprintf("GRP%s", g)] <- with(df, eval(parse(text=groups[[g]])))
    }
  }
  df <- df[,c(colnames, sprintf("GRP%s", names(groups)))]

  momoAttr$DoA <- DoA
  momoAttr$DoPR <- DoPR
  momoAttr$groups <- names(groups)
  momoAttr$models <- models
  momoAttr$hfile <- hfile
  momoAttr$country <- country
  momoAttr$source <- source
  momoAttr$WStart <- WStart
  momoAttr$WEnd <- WEnd
  momoAttr$Ysum <- Ysum
  momoAttr$Wsum <- Wsum
  momoAttr$delayCorr <- delayCorr
  momoAttr$histPer <- histPer

  attr(df, "DoA") <- DoA
  attr(df, "DoPR") <- DoPR
  attr(df, "groups") <- names(groups)
  attr(df, "models") <- models
  attr(df, "hfile") <- hfile
  attr(df, "country") <- country
  attr(df, "source") <- source
  attr(df, "WStart") <- WStart
  attr(df, "WEnd") <- WEnd
  attr(df, "Ysum") <- Ysum
  attr(df, "Wsum") <- Wsum
  attr(df, "delayCorr") <- delayCorr
  attr(df, "histPer") <- histPer

  # Automatically calculate Ydrop and Wdrop, based on DoA
  momoAttr$Ydrop <- (isoweek(DoA, "both_num") %/% 100) - (isoweek(DoA)<=21)
  momoAttr$Wdrop <- ifelse((isoweek(DoA, "both_num") %% 100)>21 & (isoweek(DoA, "both_num") %% 100)<40, 21, 40)
  attr(df, "Ydrop") <- (isoweek(DoA, "both_num") %/% 100) - (isoweek(DoA)<=21)
  attr(df, "Wdrop") <- ifelse((isoweek(DoA, "both_num") %% 100)>21 & (isoweek(DoA, "both_num") %% 100)<40, 21, 40)

  # DEATHS
  # We generate nb = total number of death known in the series
  # that will later be aggregated (indeed ==1 per record...)
  df$nb <- 1

  # To Study the delay
  # For an aggregation a specific day, week and year,
  # We know only n2: what is REGISTERED before the date chosen for the aggregation DoR >= DoPR
  df$nb2 <- NA
  df$nb2[df$DoR <= DoA] <- df$nb[df$DoR <= DoA]


  df[,c("WoDi","YoDi")] <- isoweek(df$DoD, "matrix")
  df[,c("WoRi", "YoRi")] <- isoweek(df$DoR, "matrix")
  df$YoRi[df$YoRi <= isoweek(DoPR, "year") & df$WoRi <= isoweek(DoPR, "week")] <- NA

  # DELAYS
  # diff = number of weeks between week of death and week of registration
  if (compatibility.mode) {
    df$diff <- df$WoRi - df$WoDi
    a <- df$YoRi - df$YoDi
    a[a<1 | a>5 | is.na(a)] <- 0
    df$diff <- df$diff + 52*a + as.integer(df$WoDi==53 & a>0)
  } else {
    dR <- as.POSIXlt(df$DoR)$wday; dR[dR==0] <- 7
    dD <- as.POSIXlt(df$DoD)$wday; dD[dD==0] <- 7
    df$DoR <- df$DoR - dR + 1
    df$DoD <- df$DoD - dD + 1
    df$diff <- as.integer(df$DoR-df$DoD)/7
  }

  # we must model for each week, what we know at the day of aggregation.

  # After aggregation, WRxx will be = number of death registered after xx FULL weeks among total death occured one week
  # threfore == 1 if diff <= `XX'
  # according to what we know the day of aggregation
  NoR <- as.POSIXlt(df$DoR)$wday; NoR[NoR==0] <- 7
  NoA <- as.POSIXlt(DoA)$wday; NoA[NoA==0] <- 7
  momoAttr$NoR <- NoR
  momoAttr$NoA <- NoA
  attr(df, "NoR") <- NoR
  attr(df, "NoA") <- NoA

  for (XX in 0:delayCorr) {
    df[,sprintf("WR%s",XX)] <- as.integer((df$diff <= XX | (df$diff == XX+1  & (NoR >=1 & NoR <= NoA))) & !is.na(df$nb2))
  }

  momoAttr$WOSI <- isoweek(DoA - 7, "week")
  momoAttr$YOSI <- isoweek(DoA - 7, "year")
  momoAttr$WOAI <- isoweek(DoA, "week")
  momoAttr$YOAI <- isoweek(DoA, "year")
  momoAttr$WOPRI <- isoweek(DoPR, "week")
  momoAttr$YOPRI <- isoweek(DoPR, "year")

  attr(df, "WOSI") <- isoweek(DoA - 7, "week")
  attr(df, "YOSI") <- isoweek(DoA - 7, "year")
  attr(df, "WOAI") <- isoweek(DoA, "week")
  attr(df, "YOAI") <- isoweek(DoA, "year")
  attr(df, "WOPRI") <- isoweek(DoPR, "week")
  attr(df, "YOPRI") <- isoweek(DoPR, "year")

  class(df) <- c("MOMOinput", "data.frame")
  return(df)
}
