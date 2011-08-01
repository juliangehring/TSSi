## identifyStartSites ##
setGeneric("identifyStartSites",
           function(x, threshold=1, tau=c(20, 20), neighbor=TRUE,
                    fun=subtractExpectation, multicore=TRUE, ...)
           standardGeneric("identifyStartSites")
           )

setMethod("identifyStartSites",
          signature(x="TssNorm"),
          function(x, threshold=1, tau=c(20, 20), neighbor=TRUE,
                   fun=subtractExpectation, multicore=TRUE,  ...) {

  ## get parameters
  basal <- parameters(x, "basal")
  readCol <- if(fit <- parameters(x, "fit")) "fit" else "ratio"
  tau <- rep(tau, length.out=2)
  
  ## check arguments
  .checkIdentify(threshold, tau, neighbor, fun, multicore)

  ## extract normalized data, apply for each segment
  reads <- reads(x)
  y <-
    if(.useMulticore(multicore))
      multicore::mclapply(reads, .identifyCore,
                          basal=basal, tau=tau, threshold=threshold,
                          fun=fun, readCol=readCol, neighbor=neighbor,
                          grep=.grepStrand(x), ...)
    else
      lapply(reads, .identifyCore,
             basal=basal, tau=tau, threshold=threshold, fun=fun,
             readCol=readCol, neighbor=neighbor, strand=.grepStrand(x))
  
  names(y) <- names(reads)

  tss <- lapply(y, '[[', "tss")
  dif <- lapply(y, '[[', "dif")
  reads <- mapply(cbind, reads, dif, SIMPLIFY=FALSE)

  segments <- segments(x)
  segments$nTss <- sapply(tss, nrow)

  ## store results
  pars <- c(x@parameters,
            list(tau=tau, threshold=threshold, fun=fun, readCol=readCol,
                 neighbor=neighbor))

  res <- new("TssResult",
             x, reads=reads, segments=segments, tss=tss, parameters=pars,
             timestamp=Sys.time())
  
  return(res)
}
)
