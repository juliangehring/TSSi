## identify ##
setGeneric("identify",
           function(obj, threshold=1, exppara=c(20, 20), fun=subtractExpectation,
                    cumulative=FALSE, ...)
           standardGeneric("identify")
           )

setMethod("identify",
          signature(obj="TssNorm", threshold="numeric", exppara="numeric", fun="function"),
          function(obj, threshold=1, exppara=c(20, 20), fun=subtractExpectation,
                   cumulative=FALSE, ...) {

  ## get parameter
  basal <- obj@parameter$basal

  ## extract normalized data, apply for each region
  y <- lapply(obj@data, .identifyCore,
              basal=basal, exppara=exppara, threshold=threshold, fun=fun, cumulative=cumulative)

  ## store results
  pars <- list(exppara=exppara, basal=basal, threshold=threshold, fun=fun, cumulative=cumulative)

  res <- new("TssResult",
             data=y, region=obj@region, chr=obj@chr, parameter=pars, date=date())
  
  return(res)
}
)
