## plot ##
.plotTss <- function(x, y, counts=FALSE, ratio=FALSE, fit=FALSE, expect=FALSE,
                     tss=FALSE, threshold=FALSE, rug=FALSE, legend=TRUE, baseline=TRUE,
                     extend=FALSE, ...) {
  
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
             list(col="lightgray"))
  
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
             list(pch=20, col="#F4BC7A", type="p", lwd=0.75, lty=1, cex=0.75)) ## 1

  countsArgs <- if(counts)
    .getArgs("countsArgs", args,
             list(x=start, y=reads$counts),
             list(pch=20, col="#5655A6", lwd=1, lty=1, type="p")) ## 4
  
  ratioArgs <- if(ratio)
    .getArgs("ratioArgs", args,
             list(x=start, y=reads$ratio),
             list(pch=20, col="#009A6D", lwd=1, lty=1, type="p")) ## 3

  fitArgs <- if(fit <- fit && "fit" %in% names(reads))
    .getArgs("fitArgs", args,
             list(x=start, y=reads$fit),
             list(pch=20, col="#96B74E", lwd=1, lty=1, type="p")) ## 2

  tssArgs <- if(tss)
    .getArgs("tssArgs", args,
             list(x=tss(x, y)$pos, y=tss(x, y)$reads),
             list(pch=20, cex=1.8, col="#840472", lwd=2, lty=1, type="o")) ## 5

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
    isPoint <- !(.catLegend("type", lArgs) %in% c("l", "n"))
    legend1 <- list(col=.catLegend("col", lArgs),
                    pch=ifelse(isPoint, .catLegend("pch", lArgs), NA),
                    lwd=ifelse(isLine, .catLegend("lwd", lArgs), NA),
                    lty=ifelse(isLine, .catLegend("lty", lArgs), NA))
    legend3 <- list(legend=varNames[ind], x="topleft")
    legendArgs <- .getArgs("legendArgs", args, legend1, legend3)
  }

  ## extend expect
  if(expect && extend) {
    expectArgs$x <- start[1L]:start[length(start)]
    cou <- if(fit) reads$fit else reads$ratio
    indTss <- start %in% tss(x, y)$pos
    basal <- parameters(x, "basal")
    tau <- parameters(x, "tau")
    fun <- parameters(x, "fun")
    neighbor <- parameters(x, "neighbor")
    
    cumBg <- .cumulativeReads(start, cou, indTss, basal, tau, neighbor)
    expectArgs$y <- fun(NA, cumBg, indTss, start, basal, tau, extend=TRUE)$expect
  }

  ## plot
  do.call("plot", plotArgs)
  if(baseline) do.call("abline", baselineArgs)
  if(threshold) do.call("abline", thresholdArgs)
  if(rug) do.call("rug", rugArgs)
  if(expect) do.call("points", expectArgs)
  if(tss) {
    tssArgs <- tssArgs[!names(tssArgs) %in% "type"]
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
