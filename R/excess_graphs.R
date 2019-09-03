# MOMOpack for R

# Originally MOMOpack V 4.3 for Stata,
# created by Bernadette Gergonne, SSI-EpiLife for Euro MOMO.
# Ported into R by Theodore Lytras <thlytras@gmail.com>
# Helper function to plot these graphs
plotExcessGraph <- function(data, y, x="wk", xtext="wk2", col=rainbow(length(y)), main=NULL,
                            lwd=1, lty=1, point=FALSE, line=TRUE, legend=rep(NA,length(y)), baseline=FALSE, cexf=1)
{
  # Make sure agruments lwd, point, line are sane
  suppressWarnings({
    lwd <- (rep(0, length(y)) + lwd)[1:(length(y))]
    lty <- (rep(0, length(y)) + lty)[1:(length(y))]
    line <- (rep(TRUE,length(y)) & line)[1:(length(y))]
    point <- (rep(TRUE, length(y)) & point)[1:(length(y))]
    lty[!line] <- 0
  })
  # Calculate points() type for each variable
  ptype <- rep("n",length(y))
  ptype[line] <- "l"; ptype[point] <- "p"; ptype[line & point] <- "o"
  # Set the values for the x axis and corresponding labels
  xwk <- with(data, (floor(min(get(x))/50)*50):(ceiling(max(get(x))/50)*50))
  xlabels <- as.character(data[,xtext][match(xwk, as.integer(data[,xtext]))])
  # Create the plot
  par(mar=c(8,3,3.5,3))
  with(data, {
    # Plot the rest of the variables
    plot(0, type="n", xlim=range(xwk),
         ylim=c(min(data[,y], na.rm=TRUE) , max(data[,y], na.rm=TRUE)),
         xlab=NA, ylab=NA, xaxt="n")
    axis(1, at=xwk[grep("-01|-26", xlabels)], labels=xlabels[grep("-01|-26", xlabels)])
    if (baseline) abline(h=0, col="darkred", lwd=2)
    for (i in 1:length(y)) {
      points(x=xwk, y=get(y[i])[match(xwk,get(x))], col=col[i], type=ptype[i], lwd=lwd[i], lty=lty[i], pch=20)
    }
    # Annotate the plot and make the legend
    mtext("Week number", side=1, line=2, cex=1*cexf)
    if (length(main)>0) mtext(main[1], side=3, cex=1.2*cexf, line=1.8, font=2)
    if (length(main)>1) mtext(main[2], side=3, cex=1.2*cexf, line=0.5, font=2)
    legend("top", legend=legend, col=col, inset=c(0,1.19), xpd=TRUE, ncol=3,
           seg.len=4, lwd=lwd,
           lty=lty, pch=c(NA,19)[point+1])
  })
}

excessGraphsMOMO <- function(output, dirs, group=NA) {
  VERSION <- unique(output[[1]]$finalDataset$Version)
  if (!is.na(group)) {
    output <- output[group]
    if (is.null(output[[1]])) stop("The requested group was not found in the MOMO output")
  }

  lapply(output, function(x) {
    final <- x$finalDataset
    MODEL <- unique(final$Model)
    GROUP <- unique(final$GROUP)

    # We first plot the graphs seperately,

    png(sprintf("%s/GRAPH-CRUDE-MORTALITY-%s-%s-%s-%s.png", dirs$FINAL, momoAttr$country, GROUP, momoAttr$YOSI, momoAttr$WOSI),
	width=2250, height=1000, pointsize=14, res=144)
    plotExcessGraph(data=subset(final, COND6==1), cex=1,
	y=c("nb", "DOTm", "DOTc", "Pnb", names(final)[grep("UPIb", names(final))]),
	line=TRUE, point=FALSE, lwd=3, lty=1,
	col = c("navyblue", "slateblue", "seagreen", "orange2", paste("yellow", c(3:1,3:1,3:1)[1:length(grep("UPIb", names(final)))], sep="")),
	main = c(sprintf("Mortality in %s - Age group %s years - week %s-%s", momoAttr$country, GROUP, momoAttr$WOSI, momoAttr$YOSI),
	    sprintf("Crude mortality, MOMO %s Model %s", VERSION, MODEL)),
	legend = c("Number of deaths known in the series", "Data used in the model", "Corrected number of deaths", "Baseline",
	    paste("Prediction interval +", 2*length(grep("UPIb", names(final))), " Zscores", sep="")))
    dev.off()

    png(sprintf("%s/GRAPH-STD-VARIATIONS-%s-%s-%s-%s.png", dirs$FINAL, momoAttr$country, GROUP, momoAttr$YOSI, momoAttr$WOSI),
	width=2250, height=1000, pointsize=14, res=144)
    plotExcessGraph(data=subset(final, COND6==1), cex=1,
	y=c("zscore", "DOTzm", "DOTz"),
	line=TRUE, point=FALSE, lwd=3, lty=1, baseline=TRUE,
	col = c("navyblue", "slateblue", "seagreen"),
	main = c(sprintf("Mortality in %s - Age group %s years - week %s-%s", momoAttr$country, GROUP, momoAttr$WOSI, momoAttr$YOSI),
	    "Standardized deviation from the baseline"),
	legend = c("Z-score", "Z-score on data used in the model", "Z-score on corrected data"))
    dev.off()


    # And then we plot both again on the same canvas (png file)

    png(sprintf("%s/GRAPH-COMBINED-VARIATIONS-%s-%s-%s-%s.png", dirs$FINAL, momoAttr$country, GROUP, momoAttr$YOSI, momoAttr$WOSI),
	width=2250, height=2000, pointsize=14, res=144)
    par(mfcol=c(2,1))
    plotExcessGraph(data=subset(final, COND6==1), cex=1,
	y=c("nb", "DOTm", "DOTc", "Pnb", names(final)[grep("UPIb", names(final))]),
	line=TRUE, point=FALSE, lwd=3, lty=1,
	col = c("navyblue", "slateblue", "seagreen", "orange2", paste("yellow", c(3:1,3:1,3:1)[1:length(grep("UPIb", names(final)))], sep="")),
	main = c(sprintf("Mortality in %s - Age group %s years - week %s-%s", momoAttr$country, GROUP, momoAttr$WOSI, momoAttr$YOSI),
	    sprintf("Crude mortality, MOMO %s Model %s", VERSION, MODEL)),
	legend = c("Number of deaths known in the series", "Data used in the model", "Corrected number of deaths", "Baseline",
	    paste("Prediction interval +", 2*length(grep("UPIb", names(final))), " Zscores", sep="")))
    plotExcessGraph(data=subset(final, COND6==1), cex=1,
	y=c("zscore", "DOTzm", "DOTz"),
	line=TRUE, point=FALSE, lwd=3, lty=1, baseline=TRUE,
	col = c("navyblue", "slateblue", "seagreen"),
	main = c(NA, "Standardized deviation from the baseline"),
	legend = c("Z-score", "Z-score on data used in the model", "Z-score on corrected data"))
    dev.off()

  })
}

