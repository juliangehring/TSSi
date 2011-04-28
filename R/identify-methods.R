## identify ##
setGeneric("identify",
           function(obj, threshold=1, fun=subtractExpectation, ...)
           standardGeneric("identify")
           )

setMethod("identify",
          signature(obj="TssNorm", threshold="numeric", fun="function"),
          function(obj, threshold=1, fun=subtractExpectation, ...) {

  ## get parameter
  exppara <- obj@parameter$exppara
  exppara <- obj@parameter$basal

  ## extract normalized data, apply for each region
  y <- lapply(obj@data, .identifyCore,
              basal=basal, exppara=exppara, threshold=threshold, fun=fun)

  ## store results
  pars <- list(exppara=exppara, basal=basal, threshold=threshold, fun=fun)

  res <- new("TssResult",
             data=y, region=obj@region, chr=obj@chr, parameter=pars, data=date())
  
  return(res)
}
)
