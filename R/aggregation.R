# MOMOpack for R version

# Originally MOMOpack V 4.3 for Stata,
# created by Bernadette Gergonne, SSI-EpiLife for Euro MOMO.
# Port to R and further development by Theodore Lytras <thlytras@gmail.com>
#' @import stats
aggregateMOMO <- function(mi, group, compatibility.mode=FALSE) {
  # We make the AGGREGATION by week of death
  aggr1 <- aggregate(mi[,c("nb", "nb2", colnames(mi)[grep("WR", colnames(mi), fixed=TRUE)])], by=mi[,c("YoDi","WoDi")], sum, na.rm=TRUE)
  aggr1 <- aggr1[order(aggr1$YoDi, aggr1$WoDi),]

  # Creation total number of registration
  # for the numbre of registration, we make an aggregation the week of registration
  aggr2 <- aggregate(mi[,c("nb")], by=mi[,c("YoRi","WoRi")], sum, na.rm=TRUE)
  names(aggr2)[3] <- "nbr"

  aggr3 <- merge(aggr1, aggr2, all=TRUE, by.x=c("YoDi","WoDi"), by.y=c("YoRi","WoRi"))
  #aggr3 <- aggr3[order(aggr3$YoDi, aggr3$WoDi),]

  # COMPLETION OF TIME SERIES
  # we ensure that there is no missing weeks in the series
  # by merging with a complete time series from 1960 until 2020
  time.txt <- seq.Date(as.Date("1961-1-5"), as.Date("2021-1-5"), by="week")
  time.txt <- as.data.frame(isoweek(time.txt, "matrix")[,2:1])
  names(time.txt) <- c("YoDi", "WoDi")
  aggr4 <- merge(time.txt, aggr3, all=TRUE)
  aggr4 <- aggr4[order(aggr4$YoDi, aggr4$WoDi),]


  hfile <- merge(data.frame(
      date=seq.Date(from=min(momoAttr$hfile$date),
		to=as.Date(paste(format(max(momoAttr$hfile$date), "%Y"), 12, 31, sep="-")),
		by="day")
      ), attr(mi, "hfile"), all=TRUE)

  # we generate the name of the day of the week
  # and set up days off for Saturday and Sunday
  # This can be changed for Israel and Arabic countries.
  hfile$NoW = format(hfile$date, "%u") # That's inconsistent. On data1.do, the weekday was 1-7(Mon-Sun). Now it is 0-6(Sun-Sat)
  hfile$NoW[hfile$NoW==7] <- 0

  hfile$closed[is.na(hfile$closed)] <- 0
  hfile$closed[hfile$NoW==0 | hfile$NoW==6] <- 1

  hfile$month = format(hfile$date, "%m")
  hfile$year = format(hfile$date, "%Y")

  hfile[,c("WoWi","YoWi")] <- isoweek(hfile$date, "matrix")

  hfile <- hfile[order(hfile$YoWi, hfile$WoWi),]

  # for each week we MUST compute the number of day off
  # from monday=0 to the day of aggregation= $NOA
  hfile$closedA <- NA
  hfile$closedA[hfile$NoW>0 & hfile$NoW<=momoAttr$NoA] <- hfile$closed[hfile$NoW>0 & hfile$NoW<=momoAttr$NoA]

  # number of day off (bank holiday) per ISO week
  hfile2 <- aggregate(hfile[,c("closed","closedA")], by=hfile[,c("YoWi","WoWi")], sum, na.rm=TRUE)
  hfile2 <- hfile2[order(hfile2$YoWi, hfile2$WoWi),]

  # merging with the mortality file
  aggr5 <- merge(aggr4, hfile2, all=TRUE, by.x=c("YoDi","WoDi"), by.y=c("YoWi","WoWi"))

  # we keep only the valid part of the data set
  aggr5$YoDi[is.na(aggr5$YoDi) & aggr5$YoDi<=momoAttr$YOAI+1] <- aggr5$YoDi[is.na(aggr5$YoDi) & aggr5$YoDi<=momoAttr$YOAI+1]
  aggr5$WoDi[is.na(aggr5$WoDi) & aggr5$YoDi<=momoAttr$YOAI+1] <- aggr5$WoDi[is.na(aggr5$WoDi) & aggr5$YoDi<=momoAttr$YOAI+1]
  aggr5 <- aggr5[order(aggr5$YoDi, aggr5$WoDi),]
  aggr5$YW <- paste(aggr5$YoDi, aggr5$WoDi, sep="")

  aggr5 <- aggr5[,c("YW","WoDi","YoDi","closed","closedA","nb","nb2","nbr", colnames(aggr5)[grep("WR", colnames(aggr5), fixed=TRUE)])]

  # we create the time variable of the time series
  aggr5 <- aggr5[order(aggr5$YoDi, aggr5$WoDi),]
  aggr5$wk <- 1:nrow(aggr5)

  # WEEK NUMBER to STUDY according to the date of aggregation
  # = complete ISO week, (From monday to Sunday) PRECEDING the date of aggregation
  # for week from Monday to Sunday and aggregation from Monday the week after.
  momoAttr$WEEK <- which(aggr5$YoDi==momoAttr$YOAI & aggr5$WoDi==momoAttr$WOAI) - 1
  attr(aggr5, "WEEK") <- which(aggr5$YoDi==momoAttr$YOAI & aggr5$WoDi==momoAttr$WOAI) - 1

  # GLOBAL: Week number, earlier limit for studying delay
  # accroding to the date of "perfect registration" PR
  momoAttr$PRWEEK <- which(aggr5$YoDi==momoAttr$YOPRI & aggr5$WoDi==momoAttr$WOPRI)
  attr(aggr5, "PRWEEK") <- which(aggr5$YoDi==momoAttr$YOPRI & aggr5$WoDi==momoAttr$WOPRI)


  # label for ISO weeks
  aggr5$labels <- paste(aggr5$YoDi, sprintf("%02.0f", aggr5$WoDi), sep="-")
  aggr5$wk2 <- factor(aggr5$labels)

  # $WEEK2 is the week number until which we want to model the series
  momoAttr$WEEK2 <- momoAttr$WEEK - momoAttr$delayCorr
  attr(aggr5, "WEEK2") <- attr(aggr5, "WEEK") - attr(mi, "delayCorr")

  # Add the group on the aggregate, and other attributes
  momoAttr$group <- group
  momoAttr$model <- momoAttr$models[group]
  aggr5 <- transferMOMOattributes(aggr5, mi, except=c("hfile","groups","models"))
  attr(aggr5, "group") <- group
  attr(aggr5, "model") <- attr(mi, "models")[group]


  # We clean the data set
  aggr5$nb[is.na(aggr5$nb)] <- 0
  aggr5$nb2[is.na(aggr5$nb2)] <- 0
  if (!compatibility.mode) {
    for(i in grep("^WR",names(aggr5))){
        aggr5[is.na(aggr5[,i]),i] <- 0
    }
  }

  aggr5[aggr5$wk>=(momoAttr$WEEK - momoAttr$histPer) & aggr5$wk<=(momoAttr$WEEK+1), ]

}

