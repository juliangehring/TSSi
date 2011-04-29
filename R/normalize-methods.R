## normalize ##
setGeneric("normalize",
           function(obj, fun=mean, offset=10L, basal=1e-4,
                    regpara=c(1, 1), fit=FALSE, ...)
           standardGeneric("normalize")
           )

setMethod("normalize",
          signature(obj="TssData", fun="function", offset="integer", basal="numeric",
                    regpara="numeric", fit="logical"),
          function(obj, fun=mean, offset=10L, basal=1e-4,
                   regpara=c(1, 1), fit=FALSE, ...) {

  ## calculate ratio
  maxRead <- max(sapply(obj@data, .colFun, col="start", fun=max))
  ratio <- .initialRatio(1:(maxRead+1), regpara=regpara, basal=basal)

  ## normalize each region individually
  normData <- lapply(X=obj@data, FUN=.normalize, fun=fun, offset=offset, basal=basal, ## TODO add mclapply
                     ratio=ratio, regpara=regpara, fit=fit)

  pars <- list(offset=offset, basal=basal, regpara=regpara, fit=fit)

  res <- new("TssNorm",
             data=normData, region=obj@region, chr=obj@chr, parameter=pars, date=date())

  return(res)
}
)
