#' delayMOMO_richard
#' This is the package standardly used for Norway
#' It has three parts.
#' For delay=N:2, it uses the original method.
#' For delay=1, it uses the prediction from delay=2 and WR1
#' For delay=0, it uses the prediction from delay=2 and WR0
#' @param aggr Output data from aggregateMOMO()
#' @param zvalue 95\% z-value
#' @import data.table
#' @importFrom stats glm poisson quasipoisson predict
delayMOMO_richard <- function(aggr, zvalue=1.96) {
  #aggr <- readRDS("test.RDS")
  aggrMaster <- copy(aggr)
  setDT(aggr)
  aggr <- aggr[order(aggr$wk),]

  #* Drop obs in week of aggregation
  aggr <- aggr[-nrow(aggr),]

  modellingWeeks <- MOMO::momoAttr$PRWEEK:MOMO::momoAttr$WEEK2
  modellingWeeks <- modellingWeeks[modellingWeeks > min(aggr$wk)+MOMO::momoAttr$delayCorr*2]

  aggr$sin52 <- sin(aggr$wk*2*pi/52.17857)
  aggr$cos52 <- cos(aggr$wk*2*pi/52.17857)

  aggr$sin26 <- sin(aggr$wk*2*pi/26)
  aggr$cos26 <- cos(aggr$wk*2*pi/26)

  aggr[,diff_WR1_minus_WR0:=WR1-WR0]

  # the first year needs to be the same as the second
  # the last year needs to be the same as the last
  # this is so we get stable estimates
  aggr[,modellingYear:=YoDi]
  #print(xtabs(~aggr$modellingYear))
  minYear <- min(aggr[aggr$wk %in% modellingWeeks,]$YoDi)
  maxYear <- max(aggr[aggr$wk %in% modellingWeeks,]$YoDi)
  #aggr[YoDi<=minYear,modellingYear:=minYear+1]
  #aggr[YoDi>=maxYear,modellingYear:=maxYear-1]
  aggr[,modellingYear:=modellingYear-2000]

  aggr[,summer:=WoDi %in% 21:39]
  aggr[YoDi>=maxYear,modellingYear:=maxYear-1]

  for(r in MOMO::momoAttr$delayCorr:0){
    #print(r)
    aggr[,CCC:=shift(closed,n=r)]
    aggr[,a:=get(sprintf("WR%s",r))/nb]
    aggr[,perc:=get(sprintf("WR%s",r))/nbr]

    if(r< (MOMO::momoAttr$delayCorr)){
      aggr[,shifted1:=shift(get(sprintf("pred%s",r+1)),n=1)]
    }
    if(r< (MOMO::momoAttr$delayCorr-1)){
      aggr[,shifted2:=shift(get(sprintf("pred%s",r+2)),n=2)]
    }

    if(r==0){
      form <- sprintf("nb2 ~ modellingYear+shifted2+WR%s+closed + sin52 + cos52", r)
      formLow <- sprintf("nb2 ~ modellingYear + shifted2+closed + sin52 + cos52")

      fit <- glm(as.formula(form), data=aggr[aggr$wk %in% modellingWeeks,], family=poisson)
      od <- max(1,sum(fit$weights * fit$residuals^2)/fit$df.r)
      if (od > 1) fit <- glm(as.formula(form),data=aggr[aggr$wk %in% modellingWeeks,], family=quasipoisson)
      #print(summary(fit))
      p <- predict(fit,aggr, type="response")
      if(!fit$converged | max(p,na.rm=T)>(max(aggr$nb,na.rm=T)*1.5) | min(p,na.rm=T)<(min(aggr$nb,na.rm=T)/1.5)){
        print("DOWNGRADING MODEL DUE TO ERROR")
        fit <- glm(as.formula(formLow), data=aggr[aggr$wk %in% modellingWeeks,], family=poisson)
      }
    } else if(r==1){
      form <- sprintf("nb2 ~ modellingYear+shifted1+WR%s+closed + sin52 + cos52", r)
      formLow <- sprintf("nb2 ~ modellingYear + shifted1+closed + sin52 + cos52")

      fit <- glm(as.formula(form), data=aggr[aggr$wk %in% modellingWeeks,], family=poisson)
      od <- max(1,sum(fit$weights * fit$residuals^2)/fit$df.r)
      if (od > 1) fit <- glm(as.formula(form),data=aggr[aggr$wk %in% modellingWeeks,], family=quasipoisson)
      #print(summary(fit))
      p <- predict(fit,aggr, type="response")
      if(!fit$converged | max(p,na.rm=T)>(max(aggr$nb,na.rm=T)*1.5) | min(p,na.rm=T)<(min(aggr$nb,na.rm=T)/1.5)){
        print("DOWNGRADING MODEL DUE TO ERROR")
        fit <- glm(as.formula(formLow), data=aggr[aggr$wk %in% modellingWeeks,], family=poisson)
      }
    } else {
      # original
      form1 <- sprintf("a ~ CCC + wk")
      form <- sprintf("nb2 ~ WR%s + Pa + wk",r)

      fit1 <- glm(as.formula(form1), data=aggr[aggr$wk %in% modellingWeeks,], family=poisson)
      p <- predict(fit1,aggr, type="response")
      aggr[,Pa:=p]

      fit <- glm(as.formula(form), data=aggr[aggr$wk %in% modellingWeeks,], family=poisson)
      od <- max(1,sum(fit$weights * fit$residuals^2)/fit$df.r)
      if (od > 1) fit <- glm(as.formula(form),data=aggr[aggr$wk %in% modellingWeeks,], family=quasipoisson)
    }

    p <- predict(fit,aggr, type="response")
    aggr[,(sprintf("pred%s",r)):=p]

    p <- predict(fit,aggr,se.fit=TRUE)$se.fit
    aggr[,(sprintf("stdp%s",r)):=p]

    # creating VpWRXX (theoretical)
    aggr[,(sprintf("VpWR%s",r)):=pmax(0,get(sprintf("pred%s",r))*od) + (get(sprintf("pred%s",r))*get(sprintf("stdp%s",r)))^2]

    UPIvar <- sprintf("UPI%s",r)
    LPIvar <- sprintf("LPI%s",r)

    UCIvar <- sprintf("UCI%s",r)
    LCIvar <- sprintf("LCI%s",r)

    aggr[,(UPIvar):=as.numeric(NA)]
    aggr[,(LPIvar):=as.numeric(NA)]

    aggr[,(UCIvar):=as.numeric(NA)]
    aggr[,(LCIvar):=as.numeric(NA)]

    # Prediction Interval
    aggr[,(UPIvar):=(get(sprintf("pred%s",r))^(2/3) + zvalue*((4/9)*(get(sprintf("pred%s",r))^(1/3))*(od+(get(sprintf("stdp%s",r))^2)*(get(sprintf("pred%s",r)))))^(1/2))^(3/2)]
    aggr[,(LPIvar):=(get(sprintf("pred%s",r))^(2/3) - zvalue*((4/9)*(get(sprintf("pred%s",r))^(1/3))*(od+(get(sprintf("stdp%s",r))^2)*(get(sprintf("pred%s",r)))))^(1/2))^(3/2)]

    aggr[,(UCIvar):=get(sprintf("pred%s",r)) + zvalue*get(sprintf("stdp%s",r))]
    aggr[,(LCIvar):=get(sprintf("pred%s",r)) - zvalue*get(sprintf("stdp%s",r))]
  }

  aggr[,VpWR:=0]
  aggr[,delay:=max(wk)-wk]
  for(r in 0:MOMO::momoAttr$delayCorr){
    aggr[delay==r,VpWR:=get(sprintf("VpWR%s",r))]
    aggr[delay==r,pred:=get(sprintf("pred%s",r))]
    aggr[delay==r,UPIc:=get(sprintf("UPI%s",r))]
    aggr[delay==r,LPIc:=get(sprintf("LPI%s",r))]
    aggr[delay==r,UCIc:=get(sprintf("UCI%s",r))]
    aggr[delay==r,LCIc:=get(sprintf("LCI%s",r))]
  }
  #** we generate the CORRECTED number of death
  aggr[wk<=MOMO::momoAttr$WEEK2,nbc:=nb]
  aggr[MOMO::momoAttr$WEEK2 < wk & wk<= MOMO::momoAttr$WEEK, nbc:=pmax(0,pred,nb)]

  aggr[,GROUP:=MOMO::momoAttr$group]

  aggr <- as.data.frame(aggr)
  return(aggr)
}

