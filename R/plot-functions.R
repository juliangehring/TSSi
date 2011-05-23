## plot ##
.plotTss <- function(x, y, counts=FALSE, ratio=FALSE, fit=FALSE, expect=FALSE,
                     tss=FALSE, threshold=FALSE, rug=FALSE, legend=TRUE, ...) {
  
  args <- list(...)

  ## get data
  reads <- reads(x, y)
  segmentName <- rownames(segments(x, y))
  start <- reads$start

  ## check for index
  if(missing(y) || !is.data.frame(reads))
    stop("'y' must be specified.")

  ## plot
  plot1 <- list(x=reads$start, y=reads$counts, type="n") ## TODO better
  plot3 <- list(xlab="Position", ylab="Reads", main=segmentName)
  
  do.call("plot", .getArgs("plotArgs", plot1, plot3, args))
  
  ## threshold
  if(threshold) {
    threshold1 <- list(h=parameters(x, "threshold"))
    threshold3 <- list(col="gray")
    thresholdArgs <- .getArgs("thresholdArgs", threshold1, threshold3, args)
    
    do.call("abline", thresholdArgs)
  }

  ## rug
  if(rug) {
    tssData <- tss(x, y)
    rug1 <- list(x=tssData$pos)
    rugArgs <- .getArgs("rugArgs", rug1, list(), args)
    
    do.call("rug", rugArgs)
  }

  ## expect
  expectArgs <- NULL
  if(expect) {
    expect1 <- list(x=start, y=reads$expect)
    expect3 <- list(pch=24, col=4, type="b", lwd=1, lty=1)
    expectArgs <- .getArgs("expectArgs", expect1, expect3, args)
    
    do.call("points", expectArgs)
  }  

  ## tss
  tssArgs <- NULL
  if(tss) {
    tssData <- tss(x, y)
    tss1 <- list(x=tssData$pos, y=tssData$reads)
    tss3 <- list(pch=20, lwd=2, col=1, lty=1)
    tssArgs <- .getArgs("tssArgs", tss1, tss3, args)
    
    do.call("points", c(type="h", tssArgs))
    do.call("points", c(type="p", tssArgs))
    
    tssArgs <- c(tssArgs, type="h") ## for legend
  }

  ## counts
  countsArgs <- NULL
  if(counts) {
    counts1 <- list(x=start, y=reads$counts)
    counts3 <- list(pch=21, col=1, lwd=1, lty=1, type="p")
    countsArgs <- .getArgs("countsArgs", counts1, counts3, args)
    
    do.call("points", countsArgs)
  }

  ## ratio
  ratioArgs <- NULL
  if(ratio) {
    ratio1 <- list(x=start, y=reads$ratio)
    ratio3 <- list(pch=22, col=2, lwd=1, lty=1, type="p")
    ratioArgs <- .getArgs("ratioArgs", ratio1, ratio3, args)
    
    do.call("points", ratioArgs)
  }  

  ## fit
  fitArgs <- NULL
  if(fit && "fit" %in% names(reads)) {
    fit1 <- list(x=start, y=reads$fit)
    fit3 <- list(pch=20, col=3, lwd=1, lty=1, type="p")
    fitArgs <- .getArgs("fitArgs", fit1, fit3, args)
    
    do.call("points", fitArgs)
  }

  ## legend
  if(legend) {
    lArgs <- list(expectArgs, countsArgs, ratioArgs, fitArgs, tssArgs)
    
    ind <- c(expect, counts, ratio, fit, tss)
    varNames <- c("expect", "counts", "ratio", "fit", "tss")
    isLine <- !(.catLegend("type", lArgs) %in% c("p", "n"))

    legend1 <- list(legend=varNames[ind],
                    col=.catLegend("col", lArgs),
                    pch=.catLegend("pch", lArgs),
                    lwd=ifelse(isLine, .catLegend("lwd", lArgs), NA),
                    lty=ifelse(isLine, .catLegend("lty", lArgs), NA))
    legend3 <- list(x="topleft")
    legendArgs <- .getArgs("legendArgs", legend1, legend3, args)

    do.call("legend", legendArgs)
  }
}


## getArgs ##
.getArgs <- function(name, first=NULL, last=NULL, ...) {

  ind <- which(names(...) %in% name)[1] ## [[]] allows only one element for indexing
  middle <- if(length(ind) != 0) ...[[ind]] else NULL
  args <- c(first, middle, last)
  args <- args[!duplicated(names(args))]
  
  return(args)
}


## catLegend ##
.catLegend <- function(arg, lArgs) {

  res <- unlist(sapply(lArgs, "[", arg), use.names=FALSE)

  return(res)
}
