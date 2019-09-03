# MOMOpack for R

# Originally MOMOpack V 4.3 for Stata,
# created by Bernadette Gergonne, SSI-EpiLife for Euro MOMO.
# Port to R and further development by Theodore Lytras <thlytras@gmail.com>

analyzeMOMO <- function(mi, version="v4-3", datesISO=TRUE, useAUTOMN=FALSE, USEglm2=TRUE, zvalue=1.96, compatibility.mode=FALSE, verbose=TRUE) {
  if (!("MOMOinput" %in% class(mi)))
    stop("Argument 'mi' should have class \"MOMOinput\".")
  return(lapply(momoAttr$groups, function(x) {
    if (verbose) cat(sprintf("- Iterating over group %s... ", x))
    ret <- analyzeMOMOgroup(mi, x, version, datesISO, useAUTOMN, USEglm2, zvalue, compatibility.mode)
    if (verbose) cat("DONE\n")
    ret
  }))
}

analyzeMOMOgroup <- function(mi, group, version="v4-3", datesISO=TRUE, useAUTOMN=FALSE, USEglm2=TRUE, zvalue=1.96, compatibility.mode=FALSE) {
  if (!("MOMOinput" %in% class(mi)))
    stop("Argument 'mi' should have class \"MOMOinput\".")
  if (sum(mi[,sprintf("GRP%s", group)], na.rm=TRUE) == nrow(mi)) {
    groupfile <- mi
  } else {
    groupfile <- mi[mi[,sprintf("GRP%s", group)] & !is.na(mi[,sprintf("GRP%s", group)]),]
  }
  aggr <- aggregateMOMO(groupfile, group, compatibility.mode)
  dataExport$aggr <- aggr
  aggr_fullDelay <- delayMOMO(aggr, zvalue)
  dataExport$aggr_fullDelay <- aggr_fullDelay
  aggr_delay <- trimDelayMOMO(aggr_fullDelay)
  dataExport$aggr_delay <- aggr_delay
  final <- excessMOMO(aggr=aggr_delay, version, useAUTOMN, USEglm2, zvalue)

  n <- names(final)
  extraVars <- n[stringr::str_detect(n,"^pred")]
  keepVars <- n[!n %in% extraVars]

  toSave <- final
  final <- final[,keepVars]

  for(Z in seq(4,20,2)) {
    # We drop the variables UPI if they are not crossed by the data
    if (sum(final$nbc > final[[paste("UPIb",Z,sep="")]], na.rm=TRUE)==0) {
      final[[paste("UPIb",Z,sep="")]] <- NULL ## RICHARD CHANGE
      #break
    }
  }

  table <- tableMOMO(final)
  EUROMOMO <- EUROMOMOoutput(final, useAUTOMN, datesISO)
  periods <- list(
    cumChoice = calcPeriodMOMO(final, momoAttr$WStart, momoAttr$WEnd),   # 1. using the chosen Period
    cumWinter = calcPeriodMOMO(final, 40, 20),   # 2. WINTER EXCESS WEEK 40 to 20
    cumSummer = calcPeriodMOMO(final, 21, 39),   # 3. SUMMER EXCESS WEEK 21 to 39
    cumYear = calcPeriodMOMO(final, 1, 53, attr(mi, "WStart")),   # 4. EXCESS FULL YEAR WEEK 1 to 53
    cumSeason = calcPeriodMOMO(final, 27, 26))   # 5. EXCESS FULL SEASON w27 to w26
  attr(periods, "WStart") <- momoAttr$WStart
  attr(periods, "WEnd") <- momoAttr$WEnd
  return(list(
    aggregate = aggr, aggregate_fullDelay = aggr_fullDelay, aggregate_delay = aggr_delay,
    finalDataset = final, MOMOtable=table, EUROMOMOcomplete = EUROMOMO$COMPLETE,
    EUROMOMOrestricted = EUROMOMO$RESTRICTED, periods = periods,
    toSave=toSave))

}

