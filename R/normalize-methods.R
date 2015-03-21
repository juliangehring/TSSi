## normalizeCounts ##
setGeneric("normalizeCounts",
           function(x, fun=mean, offset=10L, basal=1e-4,
                    lambda=c(0.1, 0.1), fit=FALSE, multicore=TRUE,
                    optimizer="all", ...)
           standardGeneric("normalizeCounts")
           )

setMethod("normalizeCounts",
          signature(x="TssData"),
          function(x, fun=mean, offset=10L, basal=1e-4,
                   lambda=c(0.1, 0.1), fit=FALSE, multicore=TRUE,
                   optimizer="all", ...) {

  ## expand lambda if needed
  lambda <- rep(lambda, length.out=2)

  ## match args
  optimizer <- match.arg(optimizer, c("optim", "bobyqa", "all"))

  ## check arguments
  #.checkNormalize(fun, offset, basal, lambda, fit, multicore, optimizer)

  ## calculate ratio
  reads <- reads(x)
  maxRead <- max(sapply(reads, .colFun, col="counts", fun=max))
  initial <- .initialRatio(1:(maxRead+1), lambda=lambda, basal=basal)

  ## normalize each segment individually
  normData <-
    if(.useMulticore(multicore))
      parallel::mclapply(X=reads, FUN=.normalize,
                          fun=fun, offset=offset, basal=basal, initial=initial,
                          lambda=lambda, fit=fit, optimizer=optimizer, ...)
    else
      lapply(X=reads, FUN=.normalize,
             fun=fun, offset=offset, basal=basal, initial=initial,
             lambda=lambda, fit=fit, optimizer=optimizer)

  pars <- c(x@parameters,
            list(offset=offset, basal=basal, lambda=lambda, fit=fit,
                 optimizer=optimizer))

  res <- new("TssNorm",
             x, reads=normData, parameters=pars)

  return(res)
}
)
