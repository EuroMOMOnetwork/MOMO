# MOMOpack for R

# Originally MOMOpack V 4.3 for Stata, 
# created by Bernadette Gergonne, SSI-EpiLife for Euro MOMO. 
# Ported into R by Theodore Lytras <thlytras@gmail.com>



# Function to replicate the functionality of mkspline command in stata
# (Copied from old CRAN package "ares") 
lspline <- function(var,knots=NULL,nknots=NULL,percentiles=NULL,marginal=FALSE,names=NULL)
# generate the basis for a piecewise linear spline
# var : variable
# knots : vector positions for the knots
# nknots : (optional) number of knots
# percentiles : (optional) vector of percentiles for knots positions
# marginal : (optional) marginal effect
# names : (optional) names for the new created variables
{
# initialization
n <- length(var)
if (!is.null(nknots))
    if ((nknots > n/2) || (length(knots) > n/2) || (length(percentiles) > n/2))
        stop("Less than two points between two knots")
if (is.null(nknots) && is.null(knots) && is.null(percentiles))
    stop("Either knots position, number of knots, or percentiles for the knots must be supplied")
if (sum(!is.null(nknots),!is.null(knots),!is.null(percentiles))>1)
    stop("Knots position, number of knots and percentiles are mutually excludent")
if (!is.null(knots))
	{
	if (any(knots<min(var,na.rm=TRUE)) || any(knots>max(var,na.rm=TRUE)))
		stop("At least one knot is out of bounds")
	w <- "knots"
    k <- length(knots)+1
    }
else if (!is.null(percentiles))
	{
	w <- "percentiles"
	k <- length(percentiles)+1
	}
else
	{
	w <- "nknots"
    k <- nknots+1
    }
# if knots position are not supplied then create 
if (w=="nknots")
	knots <- quantile(var,probs=seq(0,1,by=1/k)[2:k],na.rm=TRUE)
else if (w=="percentiles")
	knots <- quantile(var,probs=percentiles,na.rm=TRUE)
# else knots <- knots # just for reminder    

# creating new variables
knots <- c(knots,max(var,na.rm=TRUE))
if (marginal)
    {
    new.vars <- var
    for (i in 2:k)
        {
        tmp.var <- double(n)
        tmp.var <- pmax(0,(var-knots[i-1]))
        new.vars <- cbind(new.vars,tmp.var)
        }
    }
else
    {
    new.vars <- pmin(var,knots[1],na.rm=FALSE)
    for (i in 2:k)
        {
        tmp.var <- double(n)
        tmp.var <- pmax(pmin(var,knots[i],na.rm=FALSE),knots[i-1],na.rm=FALSE)-knots[i-1]
        new.vars <- cbind(new.vars,tmp.var)
        }
    }
# naming new variables
if (length(names) == k)
    dimnames(new.vars)[[2]] <- names
else
    dimnames(new.vars)[[2]] <- paste(deparse(substitute(var)),".",1:k,sep="")
# returning
retval <- new.vars
#class(retval) <- "lspline"
return(retval)
}



# Function to calculate ISO week & year
isoweek <- function(x, type="week") {
  alts=c("week","year","both_text","both_num","matrix")
  if(!(type %in% alts)) stop("Unknown isoweek type requested!")
  x.date<-as.Date(x)
  x.weekday<-as.integer(format(x.date,"%w"))
  x.weekday[x.weekday==0]=7
  x.nearest.thu<-x.date-x.weekday+4
  x.isoyear<-as.integer(substring(x.nearest.thu,1,4)) # Μπορεί οι πρώτες μέρες του χρόνου να ανήκουν (κατά ISO) στην προηγούμενη χρονιά!
  x.isoweek<-(as.integer(x.nearest.thu-as.Date(paste(x.isoyear,"-1-1",sep="")))%/%7)+1
  switch(type,
    week = x.isoweek,
    year = x.isoyear,
    both_text = ifelse((is.na(x.isoyear) | is.na(x.isoweek)),NA,paste(x.isoweek,x.isoyear,sep="/")),
    both_num = ifelse((is.na(x.isoyear) | is.na(x.isoweek)),NA,x.isoyear*100+x.isoweek),
    matrix = cbind(isoweek=x.isoweek, isoyear=x.isoyear)
    )
  }



# helper function to shift a vector (akin to the [_n + shift] stata syntax)
vecshift <- function(x, shift=0) {
  if (shift==0) return(x)
  if (shift>0) return(c(x[-(1:shift)], rep(NA,shift)))
  if (shift<0) return(c(rep(NA, -shift), x[1:(length(x)+shift)]))
}



# Function to replace NAs with zeros in a vector on the fly
NAto0 <- function(x) { x[is.na(x)] <- 0; x }



# Transfer attributes
transferMOMOattributes <- function(target, source, except=c()) {
  attrToKeep <- names(attributes(source))[!(names(attributes(source)) %in% c("names", "class", "row.names", except))]
  for (x in attrToKeep) attr(target, x) <- attr(source, x)
  target
}
