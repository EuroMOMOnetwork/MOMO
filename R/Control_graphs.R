# MOMOpack for R

# Originally MOMOpack V 4.3 for Stata,
# created by Bernadette Gergonne, SSI-EpiLife for Euro MOMO.
# Ported into R by Theodore Lytras <thlytras@gmail.com>

# Ηelper function to create all six control graphs
plotControlGraph <- function(data, y, x="wk", xtext="wk2", col=rainbow(length(y)), main=NULL,
                             lwd=1, lty=1, point=FALSE, line=TRUE, legend=rep(NA,length(y)), cexf=1)
{
  # Make sure agruments lwd, point, line are sane
  suppressWarnings({
    lwd <- (rep(0, length(y)-1) + lwd)[1:(length(y)-1)]
    lty <- (rep(0, length(y)-1) + lty)[1:(length(y)-1)]
    line <- (rep(TRUE,length(y)-1) & line)[1:(length(y)-1)]
    point <- (rep(TRUE, length(y)-1) & point)[1:(length(y)-1)]
    lty[!line] <- 0
  })
  # Calculate points() type for each variable
  ptype <- rep("n",length(y)-1)
  ptype[line] <- "l"; ptype[point] <- "p"; ptype[line & point] <- "o"
  # Set the values for the x axis and corresponding labels
  xwk <- with(data, (floor(min(get(x))/50)*50):(ceiling(max(get(x))/50)*50))
  xlabels <- as.character(data[,xtext][match(xwk, as.integer(data[,xtext]))])
  # Create the plot
  par(mar=c(12,3,4.5,4))
  with(data, {
    # First make the plot corresponding to the right y-axis (normally, variable "closed", which should be supplied last in argument y)
    plot(x=xwk, y=get(y[length(y)])[match(xwk,get(x))], col=col[length(y)], type="p", xlim=range(xwk),
         xlab=NA, ylab=NA, axes=FALSE, pch=19)
    axis(4, at=unique(get(y[length(y)])[match(xwk,get(x))]), las=2)
    par(new=TRUE)
    # Plot the rest of the variables
    plot(0, type="n", xlim=range(xwk),
         ylim=c(0, max(data[,y[-length(y)]], na.rm=TRUE)),
         xlab=NA, ylab=NA, xaxt="n")
    axis(1, at=xwk[grep("-01|-26", xlabels)], labels=xlabels[grep("-01|-26", xlabels)])
    abline(v=c(momoAttr$WEEK,momoAttr$WEEK2), col="red", lwd=2)
    abline(v=c(momoAttr$PRWEEK), col="green", lwd=2)
    for (i in 1:(length(y)-1)) {
      points(x=xwk, y=get(y[i])[match(xwk,get(x))], col=col[i], type=ptype[i], lwd=lwd[i], lty=lty[i], pch=20)
    }
    # Annotate the plot and make the legend
    mtext("Week number", side=1, line=2, cex=1*cexf)
    mtext("Weekly number of day off (administrations are closed)", side=4, line=2, cex=1*cexf)
    if (length(main)>0) mtext(main[1], side=3, cex=1.2*cexf, line=2.5, font=2)
    if (length(main)>1) mtext(main[2], side=3, cex=1.2*cexf, line=1.5, font=2)
    if (length(main)>2) mtext(main[3], side=3, cex=1.2*cexf, line=0.5)
    if (length(main)>3) mtext(main[4], side=1, cex=0.8*cexf, adj=0, line=10.5)
    legend("top", legend=legend, col=col, inset=c(0,1.18), xpd=TRUE, ncol=2,
           seg.len=4, lwd=c(lwd,NA),
           lty=c(lty,0), pch=c(c(NA,19)[point+1], 19))
  })
}


#' @import grDevices
#' @import graphics
controlGraphsMOMO <- function(output, dirs, group=NA) {
  VERSION <- unique(output[[1]]$finalDataset$Version)
  if (!is.na(group)) {
    output <- output[group]
    if (is.null(output[[1]])) stop("The requested group was not found in the MOMO output")
  }

  lapply(output, function(x) {
    a <- x$aggregate_fullDelay
    a <- transferMOMOattributes(a[,!(names(a) %in% c("COND1", "COND2"))], a)

    a$COND1[a$YoDi >= momoAttr$YOSI-2 & a$YoDi <= momoAttr$YOSI] <- 1
    a$COND2[a$YoDi >= momoAttr$YOSI-3 & a$YoDi <= momoAttr$YOSI] <- 1

    png(sprintf("%s/GRAPH-CONTROL-%s-%s-%s-%s.png", dirs$CONTROL, a$GROUP[1], momoAttr$country, momoAttr$YOSI, momoAttr$WOSI),
	width=2000*2, height=1200*3, pointsize=21, res=144)
    layout(matrix(1:6,ncol=2,byrow=TRUE))

    plotControlGraph(
      data=subset(a, COND1==1),
      cex=0.67,
	    y=c("nb", paste("WR", 0:momoAttr$delayCorr, sep=""), "closed"),
	    line=TRUE,
      point=FALSE,
      lwd=c(4, rep(2, momoAttr$delayCorr+1)),
      lty=1,
	    col = c("steelblue4", topo.colors(momoAttr$delayCorr+1), "lavenderblush3"),
	    main = c(
	      sprintf("CONTROL FLOW OF INFORMATION %s", VERSION),
	      sprintf("Number of Deaths - %s - Age group %s years - week %s-%s",
	              momoAttr$country, a$GROUP[1], momoAttr$WOSI, momoAttr$YOSI),
	      sprintf("Aggregation the %s, correction for the %s previous weeks",
	        format(momoAttr$DoA, format="%d - %m - %Y"), momoAttr$delayCorr),
	      "The data between the 2 red lines are not accounted for in the model in order to exclude data with too large reporting delay\n"
	      ),
	    legend = c(
	      "Number of deaths known in the series",
	      paste("Registered up to", 0:momoAttr$delayCorr, "weeks after the week of death"),
	      "Weekly number of day off (administrations are closed)"
	      )
      )

    plotControlGraph(
      data=subset(a, COND1==1),
      cex=0.67,
	    y=c("nb", "nb2", "pred", "UCIc", "LCIc", "UPIc", "LPIc", "closed"),
	    line=TRUE,
      point=c(F,F,T,F,F,F,F,T),
	    lwd=c(4, 4, 3, 2, 2, 2, 2),
      lty=c(1, 1, 1, 1, 1, 1, 1),
	    col = c("steelblue4", "indianred3", "seagreen", "sandybrown", "violet", "lightcoral", "lightblue3", "lavenderblush3"),
	    main = c(sprintf("CONTROL CORRECTION FOR DELAY %s", VERSION),
	      sprintf("Number of Deaths - %s - Age group %s years - week %s-%s",
	            momoAttr$country, a$GROUP[1], momoAttr$WOSI, momoAttr$YOSI),
	      sprintf("Aggregation the %s, correction for the %s previous weeks",
	        format(momoAttr$DoA, format="%d - %m - %Y"), momoAttr$delayCorr),
	      "The data between the 2 red lines are not accounted for in the model in order to exclude data with too large reporting delay\n"
	      ),
	    legend = c("Number of deaths known in the series",
	    "Number of deaths already registered the day of aggregation",
	    "Corrected number of deaths for the last 3 weeks",
	    "Upper 95% Confidence Interval, corrected number of deaths",
	    "Lower 95% Confidence Interval, corrected number of deaths",
	    "Upper 95% Prediction Interval, corrected number of deaths",
	    "Lower 95% Prediction Interval, corrected number of deaths",
	    "Weekly number of day off (administrations are closed)")
      )

    for (i in 0:3) { # 3 is hardcoded in the original file (Control_graphs.do). Equals to attr(a, "delayCorr")
      plotControlGraph(
        data=subset(a, COND2==1),
        cex=0.67,
	      y=c("nb", "nb2", paste(c("pred", "UCI", "LCI", "UPI", "LPI", "WR"), i, sep=""), "closed"),
	      line=TRUE,
        point=c(F,F,T,F,F,F,F,F,T),
	      lwd=c(4, 4, 3, 2, 2, 2, 2, 2, 2),
        lty=c(1, 1, 1, 1, 1, 1, 1, 1, 1),
	      col = c("steelblue4", "indianred3", "yellow3", "sandybrown", "violet", "lightcoral", "lightblue3", "seagreen", "lavenderblush3"),
	      main = c(sprintf("CONTROL QUALITY OF THE PREDICTION AT WEEK %s %s", c("OF STUDY", 1:3)[i+1], VERSION),
	        sprintf("Number of Deaths - %s - Age group %s years", momoAttr$country, a$GROUP[1]),
	        sprintf("Aggregation on %s for week %s-%s",
		      format(momoAttr$DoA, format="%d - %m - %Y"), momoAttr$WOSI, momoAttr$YOSI),
	        paste("The data between the 2 red lines are not accounted for in the model in order to exclude data with too large reporting delay.\n",
		      "This represent what can be predicted after ",
		      c("the study week only.", "one week.", "two weeks.", "three weeks.")[i+1], sep="")),
	      legend = c("Number of deaths known in the series",
  	      "Number of deaths already registered the day of aggregation",
  	      "Prediction",
  	      paste(c("UCI", "LCI", "UPI", "LPI"), i, sep=""),
  	      paste("Registered up to", i, "weeks after the week of death"),
  	      "Weekly number of day off (administrations are closed)")
        )
    }

    dev.off()
  })
}
