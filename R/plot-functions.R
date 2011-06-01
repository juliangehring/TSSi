## plot ##
.plotTss <- function(x, y, counts=FALSE, ratio=FALSE, fit=FALSE, expect=FALSE,
                     tss=FALSE, threshold=FALSE, rug=FALSE, legend=TRUE, baseline=TRUE, ...) {
  
  args <- list(...)

  ## get data
  reads <- reads(x, y)
  segmentName <- rownames(segments(x, y))
  start <- reads$start

  ## check for index
  if(missing(y) || !is.data.frame(reads))
    stop("'y' must be specified.")

  ## gather arguments
  thresholdArgs <- if(threshold)
    .getArgs("thresholdArgs", args,
             list(h=parameters(x, "threshold")),
             list(col="gray"))
  
  baselineArgs <- if(baseline)
    .getArgs("baselineArgs", args,
             list(h=0),
             list(col="black"))

  rugArgs <- if(rug)
    .getArgs("rugArgs", args,
             list(x=tss(x, y)$pos))
  
  expectArgs <- if(expect)
    .getArgs("expectArgs", args,
             list(x=start, y=reads$expect),
             list(pch=24, col=4, type="b", lwd=1, lty=1))

  countsArgs <- if(counts)
    .getArgs("countsArgs", args,
             list(x=start, y=reads$counts),
             list(pch=21, col=1, lwd=1, lty=1, type="p"))
  
  ratioArgs <- if(ratio)
    .getArgs("ratioArgs", args,
             list(x=start, y=reads$ratio),
             list(pch=22, col=2, lwd=1, lty=1, type="p"))

  fitArgs <- if(fit <- fit && "fit" %in% names(reads))
    .getArgs("fitArgs", args,
             list(x=start, y=reads$fit),
             list(pch=20, col=3, lwd=1, lty=1, type="p"))

  tssArgs <- if(tss)
    .getArgs("tssArgs", args,
             list(x=tss(x, y)$pos, y=tss(x, y)$reads),
             list(pch=20, lwd=2, col=1, lty=1))

  ## collect args
  lArgs <- list(expectArgs, countsArgs, ratioArgs, fitArgs, tssArgs)
  ind <- c(expect, counts, ratio, fit, tss)

  ylim <- c(0, max(.catLegend("y", lArgs[ind])))
  plotArgs <- .getArgs("plotArgs", args,
                       list(x=start, y=reads$counts, type="n"),
                       list(xlab="Position", ylab="Reads", ylim=ylim, main=segmentName))

  ## legend
  if(legend <- legend && any(ind)) {
    varNames <- c("expect", "counts", "ratio", "fit", "tss")
    isLine <- !(.catLegend("type", lArgs) %in% c("p", "n"))
    legend1 <- list(col=.catLegend("col", lArgs),
                    pch=.catLegend("pch", lArgs),
                    lwd=ifelse(isLine, .catLegend("lwd", lArgs), NA),
                    lty=ifelse(isLine, .catLegend("lty", lArgs), NA))
    legend3 <- list(legend=varNames[ind], x="topleft")
    legendArgs <- .getArgs("legendArgs", args, legend1, legend3)
  }

  ## plot
  do.call("plot", plotArgs)
  if(baseline) do.call("abline", baselineArgs)
  if(threshold) do.call("abline", thresholdArgs)
  if(rug) do.call("rug", rugArgs)
  if(expect) do.call("points", expectArgs)
  if(tss) {    
    do.call("points", c(type="h", tssArgs))
    do.call("points", c(type="p", tssArgs))
    tssArgs <- c(tssArgs, type="h") ## for legend
  }
  if(counts) do.call("points", countsArgs)
  if(ratio) do.call("points", ratioArgs)
  if(fit) do.call("points", fitArgs)

  if(legend) do.call("legend", legendArgs)
}


## getArgs ##
.getArgs <- function(name, args, first=NULL, last=NULL) {

  ind <- which(names(args) %in% name)[1] ## [[]] allows only one element for indexing
  middle <- if(length(ind) != 0) args[[ind]] ## NULL else
  res <- c(first, middle, last)
  res <- res[!duplicated(names(res))]
  
  return(res)
}


## catLegend ##
.catLegend <- function(arg, lArgs) {

  res <- unlist(sapply(lArgs, "[", arg), use.names=FALSE)

  return(res)
}
