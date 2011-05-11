## identify ##
setGeneric("identify",
           function(obj, threshold=1, exppara=c(20, 20), fun=subtractExpectation,
                    cumulative=FALSE, ...)
           standardGeneric("identify")
           )

setMethod("identify",
          signature(obj="TssNorm"),
          function(obj, threshold=1, exppara=c(20, 20), fun=subtractExpectation,
                   cumulative=FALSE, ...) {

  ## get parameter
  basal <- obj@parameters$basal

  ## extract normalized data, apply for each region
  y <- lapply(obj@reads, .identifyCore,
              basal=basal, exppara=exppara, threshold=threshold, fun=fun, cumulative=cumulative)
  names(y) <- names(obj@reads)

  tss <- lapply(y, '[[', "tss")
  dif <- lapply(y, '[[', "dif")
  reads <- mapply(cbind, obj@reads, dif, SIMPLIFY=FALSE)

  ## store results
  pars <- c(obj@parameters,
            list(exppara=exppara, basal=basal, threshold=threshold, fun=fun, cumulative=cumulative))

  res <- new("TssResult",
             obj, reads=reads, tss=tss, parameters=pars, timestamp=date())
  
  return(res)
}
)
