## plot ##
.plotTss <- function(x, y, counts=FALSE, ratio=FALSE, fit=FALSE, expect=FALSE,
                     tss=FALSE, threshold=FALSE, rug=FALSE, ...) {
  
  args <- list(...)

  ## get data
  reads <- reads(x, y)
  regionName <- rownames(regions(x, y))
  start <- reads$start

  ## check for index
  if(missing(y) || !is.data.frame(reads))
    stop("'y' must be specified.")

  ## plot
  plot1 <- list(x=reads$start, y=reads$counts, type="n") ## TODO better
  plot3 <- list(xlab="Position", ylab="Reads", main=regionName)  
  do.call("plot", .getArgs("plotArgs", plot1, plot3, args))
  
  ## threshold
  if(threshold) {
    threshold1 <- list(h=parameters(x, "threshold"))
    threshold3 <- list(col="gray")
    do.call("abline", .getArgs("thresholdArgs", threshold1, threshold3, args))
  }

  ## rug
  if(rug) {
    tssData <- tss(x, y)
    rug1 <- list(x=tssData$pos)
    do.call("rug", .getArgs("rugArgs", rug1, list(), args))
  }

  ## expect
  if(expect) {
    expect1 <- list(x=start, y=reads$expect)
    expect3 <- list(pch=24, col=4, type="b")
    do.call("points", .getArgs("expectArgs", expect1, expect3, args))
  }  

  ## tss
  if(tss) {
    tssData <- tss(x, y)
    tss1 <- list(x=tssData$pos, y=tssData$counts)
    tss3 <- list(pch=20, lwd=2)
    do.call("points", .getArgs("tssArgs", tss1, c(tss3, type="h"), args))
    do.call("points", .getArgs("tssArgs", tss1, c(tss3, type="p"), args))
  }

  ## counts
  if(counts) {
    counts1 <- list(x=start, y=reads$counts)
    counts3 <- list(pch=21, col=1)
    do.call("points", .getArgs("countsArgs", counts1, counts3, args))
  }

  ## ratio
  if(ratio) {
    ratio1 <- list(x=start, y=reads$ratio)
    ratio3 <- list(pch=22, col=2)
    do.call("points", .getArgs("ratioArgs", ratio1, ratio3, args))
  }  

  ## fit
  if(fit && "fit" %in% names(reads)) {
    fit1 <- list(x=start, y=reads$fit)
    fit3 <- list(pch=20, col=3)
    do.call("points", .getArgs("fitArgs", fit1, fit3, args))
  }
}
