#' Delay adjustment as originally coded
#' but with a sin curve over time.
#' @param aggr Output data from aggregateMOMO()
#' @param zvalue 95\% z-value
#' @importFrom stats glm
#' @export delayMOMO_sincos
delayMOMO_sincos <- function(aggr, zvalue=1.96) {
  aggr <- aggr[order(aggr$wk),]

  # the period of registration for a death week XX
  aggr$closed0 <- aggr$closed + vecshift(aggr$closedA, 1)
  for (VV in 1:attr(aggr, "delayCorr")) {
    aggr[[paste("closed", VV, sep="")]] <- aggr[[paste("closed", VV-1, sep="")]] + vecshift(aggr$closed0, -VV)
  }

  # CORRECTION FOR DELAY
  # FIRST we model what we know about the previous week.
  aggr$VpWR <- 0
  aggr$pred <- NA
  aggr$UCIc <- NA
  aggr$LCIc <- NA
  aggr$UPIc <- NA
  aggr$LPIc <- NA
  aggr$GROUP <- attr(aggr, "group")

  aggr$sin52 <- sin(aggr$wk*2*pi/52.17857)
  aggr$cos52 <- cos(aggr$wk*2*pi/52.17857)

  for (XX in 0:attr(aggr, "delayCorr")) {
    aggr[[paste("CCC", XX, sep="")]] <- vecshift(aggr[[paste("closed", XX, sep="")]], XX)
    aggr[[paste("a", XX, sep="")]] <- ifelse((aggr$wk>attr(aggr, "PRWEEK") & aggr$wk<=attr(aggr, "WEEK2")), (aggr[[paste("WR", XX, sep="")]]/aggr$nb), NA)

    m1 <- suppressWarnings(glm(as.formula(paste("a", XX, " ~ CCC", XX, " + wk + sin52 + cos52", sep="")), data=subset(aggr, wk>attr(aggr, "PRWEEK") & wk<attr(aggr, "WEEK2")), family=binomial))
    aggr[[paste("Pa", XX, sep="")]] <- predict(m1, aggr, type="response")
    aggr[[paste("Pa", XX, sep="")]][which(aggr$wk<=attr(aggr, "PRWEEK") | aggr$wk>attr(aggr, "WEEK"))] <- NA

    aggr[[paste("temp", XX, sep="")]] <- aggr[[paste("WR", XX, sep="")]] / aggr[[paste("Pa", XX, sep="")]]

    m1 <- glm(as.formula(paste("nb2 ~ WR", XX, " + Pa", XX, " + wk + sin52 + cos52", sep="")), data=subset(aggr, wk>attr(aggr, "PRWEEK") & wk<attr(aggr, "WEEK2")), family=poisson)
    od <- max(1,sum(m1$weights * m1$residuals^2)/m1$df.r)
    if (od > 1) m1 <- glm(as.formula(paste("nb2 ~ WR", XX, " + Pa", XX, " + wk + sin52 + cos52", sep="")), data=subset(aggr, wk>attr(aggr, "PRWEEK") & wk<attr(aggr, "WEEK2")), family=quasipoisson)

    aggr[[paste("pred", XX, sep="")]] <- predict(m1, aggr, type="response")
    aggr[[paste("stdp", XX, sep="")]] <- predict(m1, aggr, se.fit=TRUE)$se.fit
    aggr[[paste("stdp", XX, sep="")]][aggr$wk<=attr(aggr, "PRWEEK") | aggr$wk>attr(aggr, "WEEK")-XX] <- NA
    # creating VpWRXX (theoretical)
    aggr[[paste0("VpWR",XX)]] <- pmax(0,(aggr[[paste0("pred", XX)]])*od) + ((aggr[[paste0("pred", XX)]])*aggr[[paste0("stdp", XX)]])^2
    # creating VpWRXX (empirical)
    #temp <- aggr[aggr$wk>attr(aggr, "PRWEEK") & aggr$wk<=attr(aggr, "WEEK2"),c(paste0("pred", XX),"nb")]
    #aggr[[paste0("eVpWR",XX)]] <- var(temp[[paste0("pred", XX)]]-temp$nb,na.rm=T)

    aggr[[paste("N", XX, sep="")]] <- sum(!is.na(aggr[[paste("stdp", XX, sep="")]]))
    aggr[[paste("temp", XX, sep="")]] <- NULL

    # Prediction Interval
    aggr[[paste("UPI", XX, sep="")]] <- (aggr[[paste("pred", XX, sep="")]]^(2/3) + zvalue*((4/9)*(aggr[[paste("pred", XX, sep="")]]^(1/3))*(od+(aggr[[paste("stdp", XX, sep="")]]^2)*(aggr[[paste("pred", XX, sep="")]])))^(1/2))^(3/2)
    aggr[[paste("LPI", XX, sep="")]] <- (aggr[[paste("pred", XX, sep="")]]^(2/3) - zvalue*((4/9)*(aggr[[paste("pred", XX, sep="")]]^(1/3))*(od+(aggr[[paste("stdp", XX, sep="")]]^2)*(aggr[[paste("pred", XX, sep="")]])))^(1/2))^(3/2)

    aggr[[paste("UCI", XX, sep="")]] <- aggr[[paste("pred", XX, sep="")]] + zvalue*aggr[[paste("stdp", XX, sep="")]]
    aggr[[paste("LCI", XX, sep="")]] <- aggr[[paste("pred", XX, sep="")]] - zvalue*aggr[[paste("stdp", XX, sep="")]]

    aggr$pred[aggr$wk == attr(aggr, "WEEK")-XX] <- aggr[[paste("pred", XX, sep="")]][aggr$wk == attr(aggr, "WEEK")-XX]
    aggr$UCIc[aggr$wk == attr(aggr, "WEEK")-XX] <- aggr[[paste("UCI", XX, sep="")]][aggr$wk == attr(aggr, "WEEK")-XX]
    aggr$LCIc[aggr$wk == attr(aggr, "WEEK")-XX] <- aggr[[paste("LCI", XX, sep="")]][aggr$wk == attr(aggr, "WEEK")-XX]
    aggr$UPIc[aggr$wk == attr(aggr, "WEEK")-XX] <- aggr[[paste("UPI", XX, sep="")]][aggr$wk == attr(aggr, "WEEK")-XX]
    aggr$LPIc[aggr$wk == attr(aggr, "WEEK")-XX] <- aggr[[paste("LPI", XX, sep="")]][aggr$wk == attr(aggr, "WEEK")-XX]
    aggr$VpWR[aggr$wk == attr(aggr, "WEEK")-XX] <- aggr[[paste("VpWR", XX, sep="")]][aggr$wk == attr(aggr, "WEEK")-XX]

    aggr[[paste("UCI", XX, sep="")]][aggr$wk < attr(aggr, "WEEK2")] <- NA
    aggr[[paste("LCI", XX, sep="")]][aggr$wk < attr(aggr, "WEEK2")] <- NA
    aggr[[paste("UPI", XX, sep="")]][aggr$wk < attr(aggr, "WEEK2")] <- NA
    aggr[[paste("LPI", XX, sep="")]][aggr$wk < attr(aggr, "WEEK2")] <- NA

  }

  # we generate the CORRECTED number of death
  aggr$nbc[aggr$wk < attr(aggr, "WEEK2")] <- aggr$nb[aggr$wk < attr(aggr, "WEEK2")]
  aggr$nbc[aggr$wk >= attr(aggr, "WEEK2") & aggr$wk <= attr(aggr, "WEEK")] <- pmax(aggr$pred[aggr$wk >= attr(aggr, "WEEK2") & aggr$wk <= attr(aggr, "WEEK")], aggr$nb[aggr$wk >= attr(aggr, "WEEK2") & aggr$wk <= attr(aggr, "WEEK")], na.rm=TRUE)

  #* Drop obs in week of aggregation # NEW!!
  aggr <- aggr[-nrow(aggr),]

  return(aggr)
}
