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

  ## get parameter
  basal <- obj@parameters$basal

  ## extract normalized data, apply for each region
  y <-
    if(.useMulticore(multicore))
      multicore::mclapply(obj@reads, .identifyCore,
                          basal=basal, exppara=exppara, threshold=threshold,
                          fun=fun)
    else
      lapply(obj@reads, .identifyCore,
             basal=basal, exppara=exppara, threshold=threshold, fun=fun)
  
  names(y) <- names(obj@reads)

  tss <- lapply(y, '[[', "tss")
  dif <- lapply(y, '[[', "dif")
  reads <- mapply(cbind, obj@reads, dif, SIMPLIFY=FALSE)

  ## store results
  pars <- c(obj@parameters,
            list(exppara=exppara, basal=basal, threshold=threshold, fun=fun))

  res <- new("TssResult",
             obj, reads=reads, tss=tss, parameters=pars, timestamp=Sys.time())
  
  return(res)
}
)
