## identifyStartSites ##
setGeneric("identifyStartSites",
           function(x, threshold=1, tau=c(20, 20), neighbor=TRUE,
                    fun=subtractExpectation, multicore=TRUE, ...)
           standardGeneric("identifyStartSites")
           )

setMethod("identifyStartSites",
          signature(x="TssData"),
          function(x, threshold=1, tau=c(20, 20), neighbor=TRUE,
                   fun=subtractExpectation, multicore=TRUE,  ...) {

  ## get parameters
  basal <- parameters(x, "basal")
  ## choose data column
  reads <- reads(x)
  dataNames <- c("fit", "ratio", "counts")
  readCol <- (dataNames[dataNames %in% names(reads[[1]])])[1]
  tau <- rep(tau, length.out=2)
  
  ## check arguments
  #.checkIdentify(threshold, tau, neighbor, fun, multicore)

  ## extract normalized data, apply for each segment
  y <-
    if(.useMulticore(multicore))
      parallel::mclapply(seq_along(reads), .identifyCore, reads,
                          basal=basal, tau=tau, threshold=threshold,
                          fun=fun, readCol=readCol, neighbor=neighbor,
                          strand=.grepStrand(x), ...)
    else
      lapply(seq_along(reads), .identifyCore, reads,
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
             x, reads=reads, segments=segments, tss=tss, parameters=pars)
  
  return(res)
}
)
