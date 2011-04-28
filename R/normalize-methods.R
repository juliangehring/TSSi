## normalize ##
setGeneric("normalize",
           function(obj, fun=mean, offset=10L, basal=1e-4,
                    regpara=c(1, 1), fit=FALSE, ...)
           standardGeneric("normalize")
           )

setMethod("normalize",
          signature(obj="TssData", fun="function", offset="integer", basal="numeric",
                    regpara="numeric", fit="logical"),
          function(obj, fun=mean, offset=10, basal=1e-4,
                   regpara=c(1, 1), fit=FALSE, ...) {


  ## calculate ratio
  findMaxRead <- function(x, col="start") max(x[col, ])
  maxRead <- max(sapply(x=obj@data, findMaxRead, col="start"))
  ratio <- .initialRatio(1:(maxRead+1), regpara=regpara, basal=basal)

  ## normalize each region individually
  normData <- lapply(x=obj@data, FUN=zNorm, offset=offset, basal=basal, ## add mclapply
                     ratio=ratio, regpara=regpara, fit=fit)

  pars <- list(offset=offset, basal=basal, ratio=ratio, regpara=regpara, fit=fit)

  res <- new("TssNorm",
             data=normData, region=obj@region, chr=obj@chr, parameter=pars, data=date())

  return(res)
}
)
