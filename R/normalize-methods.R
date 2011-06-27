## normalize ##
setGeneric("normalize",
           function(obj, fun=mean, offset=10L, basal=1e-4,
                    lambda=c(1, 1), fit=FALSE, multicore=TRUE,
                    optimizer="all", ...)
           standardGeneric("normalize")
           )

setMethod("normalize",
          signature(obj="TssData"),
          function(obj, fun=mean, offset=10L, basal=1e-4,
                   lambda=c(1, 1), fit=FALSE, multicore=TRUE,
                   optimizer="all", ...) {

  ## expand lambda if needed
  lambda <- rep(lambda, length.out=2)

  ## match args
  optimizer <- match.arg(optimizer, c("optim", "bobyqa", "all"))

  ## check arguments
  .checkNormalize(fun, offset, basal, lambda, fit, multicore, optimizer)

  ## calculate ratio
  maxRead <- max(sapply(obj@reads, .colFun, col="counts", fun=max))
  initial <- .initialRatio(1:(maxRead+1), lambda=lambda, basal=basal)

  ## normalize each segment individually
  normData <-
    if(.useMulticore(multicore))
      multicore::mclapply(X=obj@reads, FUN=.normalize,
                          fun=fun, offset=offset, basal=basal, initial=initial,
                          lambda=lambda, fit=fit, optimizer=optimizer, ...)
    else
      lapply(X=obj@reads, FUN=.normalize,
             fun=fun, offset=offset, basal=basal, initial=initial,
             lambda=lambda, fit=fit, optimizer=optimizer)

  pars <- list(offset=offset, basal=basal, lambda=lambda, fit=fit, optimizer=optimizer)

  res <- new("TssNorm",
             obj, reads=normData, parameters=pars, timestamp=Sys.time())

  return(res)
}
)
