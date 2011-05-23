## identify ##
setGeneric("identify",
           function(obj, threshold=1, exppara=c(20, 20),
                    fun=subtractExpectation, multicore=TRUE, ...)
           standardGeneric("identify")
           )

setMethod("identify",
          signature(obj="TssNorm"),
          function(obj, threshold=1, exppara=c(20, 20),
                   fun=subtractExpectation, multicore=TRUE,  ...) {

  ## get parameters
  basal <- parameters(obj, "basal")
  readCol <- if(fit <- parameters(obj, "fit")) "fit" else "ratio"

  ## extract normalized data, apply for each region
  y <-
    if(.useMulticore(multicore))
      multicore::mclapply(obj@reads, .identifyCore,
                          basal=basal, exppara=exppara, threshold=threshold,
                          fun=fun, readCol=readCol, ...)
    else
      lapply(obj@reads, .identifyCore,
             basal=basal, exppara=exppara, threshold=threshold, fun=fun,
             readCol=readCol)
  
  names(y) <- names(obj@reads)

  tss <- lapply(y, '[[', "tss")
  dif <- lapply(y, '[[', "dif")
  reads <- mapply(cbind, obj@reads, dif, SIMPLIFY=FALSE)

  regions <- regions(obj)
  regions$nTss <- sapply(tss, nrow)

  ## store results
  pars <- c(obj@parameters,
            list(exppara=exppara, threshold=threshold, fun=fun))

  res <- new("TssResult",
             obj, reads=reads, regions=regions, tss=tss, parameters=pars,
             timestamp=Sys.time())
  
  return(res)
}
)
