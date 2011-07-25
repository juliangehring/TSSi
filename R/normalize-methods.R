## normalizeCounts ##
setGeneric("normalizeCounts",
           function(x, fun=mean, offset=10L, basal=1e-4,
                    lambda=c(1, 1), fit=FALSE, multicore=TRUE,
                    optimizer="all", ...)
           standardGeneric("normalizeCounts")
           )

setMethod("normalizeCounts",
          signature(x="TssData"),
          function(x, fun=mean, offset=10L, basal=1e-4,
                   lambda=c(1, 1), fit=FALSE, multicore=TRUE,
                   optimizer="all", ...) {

  ## match args
  optimizer <- match.arg(optimizer, c("optim", "bobyqa", "all"))

  ## calculate ratio
  maxRead <- max(sapply(x@reads, .colFun, col="counts", fun=max))
  initial <- .initialRatio(1:(maxRead+1), lambda=lambda, basal=basal)

  ## normalize each segment individually
  normData <-
    if(.useMulticore(multicore))
      multicore::mclapply(X=x@reads, FUN=.normalize,
                          fun=fun, offset=offset, basal=basal, initial=initial,
                          lambda=lambda, fit=fit, optimizer=optimizer, ...)
    else
      lapply(X=x@reads, FUN=.normalize,
             fun=fun, offset=offset, basal=basal, initial=initial,
             lambda=lambda, fit=fit, optimizer=optimizer)

  pars <- list(offset=offset, basal=basal, lambda=lambda, fit=fit, optimizer=optimizer)

  res <- new("TssNorm",
             x, reads=normData, parameters=pars, timestamp=Sys.time())

  return(res)
}
)
