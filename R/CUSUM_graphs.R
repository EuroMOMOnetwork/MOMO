# MOMOpack for R

# Originally MOMOpack V 4.3 for Stata,
# created by Bernadette Gergonne, SSI-EpiLife for Euro MOMO.
# Ported into R by Theodore Lytras <thlytras@gmail.com>

CUSUMgraphsMOMO <- function(output, dirs, group=NA) {
  VERSION <- unique(output[[1]]$finalDataset$Version)
  if (!is.na(group)) {
    output <- output[group]
    if (is.null(output[[1]])) stop("The requested group was not found in the MOMO output")
  }

  lapply(output, function(x) {

    final <- transferMOMOattributes(subset(x$finalDataset, season>=2006), x$finalDataset)
    GROUP <- final$GROUP

    final$Sweek[final$Sweek >= 27 & final$YoDi !=2010] <- final$Sweek[final$Sweek >= 27 & final$YoDi !=2010] + 1
    final$Sweek[final$WoDi == 53] <- 27

    # Cumulative sums

    final <- final[order(final$season),]

    final$sum <- final$excess
    final$sum[is.na(final$sum)] <- 0
    final$sum <- cumsum(final$sum)

    final$Zsum <- final$zscore
    final$Zsum[is.na(final$Zsum)] <- 0
    final$Zsum <- cumsum(final$Zsum)


    # CUSUM Z-score


    # Helper function for all the graphs below
    plotCusumGraph <- function(data, y, x="wk", xtext="wk2", col=rainbow(length(y)),
	    note=NA, ylab=NA, lwd=1, lty=1, legend=NULL, tbmar=c(2,8), yshifts=c(1.2, 6),
	    plotCUM=NA, baseline=FALSE, cexf=1)
    {
      # Make sure agruments lwd, point, line are sane
      suppressWarnings({
	lwd <- (rep(0, length(y)) + lwd)[1:(length(y))]
	lty <- (rep(0, length(y)) + lty)[1:(length(y))]
      })
      # Set the values for the x axis and corresponding labels
      xwk <- with(data, (floor(min(get(x))/50)*50):(ceiling(max(get(x))/50)*50))
      xlabels <- as.character(data[,xtext][match(xwk, as.integer(data[,xtext]))])
      # Create the plot
      par(mar=c(tbmar[2],8,tbmar[1],2))
      with(data, {
	plot(0, type="n", xlim=range(xwk),
	  ylim=c(min(0, min(data[,y], na.rm=TRUE)) , max(data[,y], na.rm=TRUE)),
	  xlab=NA, ylab=NA, xaxt="n")
	axis(1, at=xwk[grep("-01|-26", xlabels)], labels=xlabels[grep("-01|-26", xlabels)])
	if (baseline) abline(h=0, col="darkred", lwd=2)
	if (!is.na(plotCUM)) abline(v=plotCUM, col="orange", lwd=2)
	for (i in 1:length(y)) {
	  points(x=xwk, y=get(y[i])[match(xwk,get(x))], col=col[i], type="l", lwd=lwd[i], lty=lty[i])
	}
	# Annotate the plot and make the legend
	mtext("Week number", side=1, line=2, cex=1*cexf)
	mtext(ylab, side=2, line=2.5, cex=2.5*cexf)
	mtext(note, side=1, cex=1.3*cexf, line=yshifts[2], adj=0)
	if (!is.null(legend)) legend("top", legend=legend, col=col, inset=c(0,yshifts[1]),
	      xpd=TRUE, ncol=min(3,length(legend)), seg.len=4, lwd=lwd, lty=lty)
      })
    }



    png(sprintf("%s/GRAPH-COMBINED-CUSUM-%s-%s-%s-%s.png", dirs$CUMULATIVE, momoAttr$country, GROUP, momoAttr$YOSI, momoAttr$WOSI),
	width=3750, height=5000, pointsize=21, res=144)
    par(mfrow=c(5,1), oma=c(1,0,9,0))

    # graph CUSUM
    plotCusumGraph(data=subset(final, COND6==1), cex=0.67,
	y=c("CUSUM", "h"),lwd=3, lty=1, tbmar=c(2,9),
	ylab="Standardised\nCUSUM", plotCUM=momoAttr$CUM,
	note=paste("The orange line represent the starting week of the CUSUM.",
	    "Current parameters set to detect a sustained shift of 1.5 SD over 3 weeks",
	    "with 5 % of false alarm = one false alarm randomly every 20 weeks", sep="\n"),
	col = c("navyblue", "red", "seagreen", "orange2"),
	legend = c("CUSUM", "decision limit"))

    # graph series
    plotCusumGraph(data=subset(final, COND6==1), cex=0.67,
	y=c("nbc", "DOTm", "DOTc", "Pnb", names(final)[grep("UPIb", names(final))]),lwd=3, lty=1,
	ylab="Mortality\nseries",
	col = c("navyblue", "slateblue", "seagreen", "orange2", paste("yellow", c(3:1,3:1,3:1,3:1,3:1)[1:length(grep("UPIb", names(final)))], sep="")),
	legend = c("Number of deaths", "Data used in the model", "Corrected number of deaths", "Baseline",
	    paste("Prediction interval +", 2*length(grep("UPIb", names(final))), " Zscores", sep="")))

    # graph cumulative sum
    plotCusumGraph(data=subset(final, COND6==1), cex=0.67,
	y="sum", lwd=3, lty=1, ylab="Cumulative\nnumber of death", col="navyblue", baseline=TRUE, tbmar=c(3,5))

    # graph Z-score
    plotCusumGraph(data=subset(final, COND6==1), cex=0.67,
	y=c("zscore", "DOTzm", "DOTz"), lwd=3, lty=1,
	ylab="Z-score\nseries",
	col = c("navyblue", "slateblue", "seagreen"), baseline=TRUE,
	legend = c("Z-score", "Z-score on data used in the model", "Z-score on corrected data"))

    # graph cumulative Z-score
    plotCusumGraph(data=subset(final, COND6==1), cex=0.67,
	y="Zsum", lwd=3, lty=1, ylab="Cumulative\nZ-score", col="navyblue", baseline=TRUE, tbmar=c(3,5))

    mtext(sprintf("ANALYSIS CUMULATIVE SUMS\n%s-%s years-%s-%s\nCUSUM set up at 0 week %s-%s", momoAttr$country, GROUP, momoAttr$YOSI, momoAttr$WOSI, momoAttr$YOSI, momoAttr$WOSI),
	side=3, outer=TRUE, cex=2)

    dev.off()

  })
}

