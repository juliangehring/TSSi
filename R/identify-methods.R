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

  ## extract normalized data, apply for each segment
  y <-
    if(.useMulticore(multicore))
      multicore::mclapply(x@reads, .identifyCore,
                          basal=basal, tau=tau, threshold=threshold,
                          fun=fun, readCol=readCol, neighbor=neighbor,
                          ...)
    else
      lapply(x@reads, .identifyCore,
             basal=basal, tau=tau, threshold=threshold, fun=fun,
             readCol=readCol, neighbor=neighbor)
  
  names(y) <- names(x@reads)

  tss <- lapply(y, '[[', "tss")
  dif <- lapply(y, '[[', "dif")
  reads <- mapply(cbind, x@reads, dif, SIMPLIFY=FALSE)

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
