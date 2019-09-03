#' Creates a scale
#' @param d a
#' @importFrom stats sd
#' @export ScaleCreate
ScaleCreate <- function(d){
  means <- apply(d,2,mean,na.rm=T)
  sds <- apply(d,2,sd,na.rm=T)

  retval <- list(
    m=means,
    sd=sds
  )
  class(retval) <- "Scale"

  return(retval)
}

#' Applies as scale
#' @param d a data frame
#' @param s scale
#' @import data.table
#' @export ScaleApply
ScaleApply <- function(d,s){
  if(!"Scale" %in% class(s)) stop("s is not a Scale")
  if(data.table::is.data.table(d)){
    for(i in 1:ncol(d)){
      n <- names(d)[i]
      d[,(n):=(get(n)-s$m[i])/s$sd[i]]
    }
  } else if(is.data.frame(d)){
    for(i in 1:ncol(d)){
      d[,i] <- (d[,i]-s$m[i])/s$sd[i]
    }
  } else if(is.matrix(d)){
    for(i in 1:ncol(d)){
      d[,i] <- (d[,i]-s$m[i])/s$sd[i]
    }
  } else (
    stop("is not data.table, data.frame, or matrix")
  )


  return(d)
}
