## plot ##
setGeneric("plot",
           function(x, y, ...)
           standardGeneric("plot")
           )


setMethod("plot",
          signature(x="TssData"),
          function(x, y, counts=TRUE, ...) {

  .plotTss(x, y, counts=counts, ...)
}
)


setMethod("plot",
          signature(x="TssNorm"),
          function(x, y, counts=TRUE, ratio=TRUE, fit=TRUE, ...) {

  .plotTss(x, y, counts=counts, ratio=ratio, fit=fit, ...)
}
)


setMethod("plot",
          signature(x="TssResult"),
          function(x, y, counts=TRUE, ratio=TRUE, fit=TRUE, expect=FALSE,
                   tss=TRUE, threshold=TRUE, rug=TRUE, ...) {

  .plotTss(x, y, counts=counts, ratio=ratio, fit=fit, expect=expect, tss=tss,
           threshold=threshold, rug=rug, ...)
}
)
