## plot ##
setMethod("plot",
          signature(x="TssData"),
          function(x, y, counts=TRUE, legend=TRUE, ...) {

  .plotTss(x, y, counts=counts, legend=legend, ...)
}
)


setMethod("plot",
          signature(x="TssNorm"),
          function(x, y, counts=TRUE, ratio=TRUE, fit=TRUE, legend=TRUE, ...) {

  .plotTss(x, y, counts=counts, ratio=ratio, fit=fit, legend=legend, ...)
}
)


setMethod("plot",
          signature(x="TssResult"),
          function(x, y, counts=TRUE, ratio=TRUE, fit=TRUE, expect=FALSE,
                   tss=TRUE, threshold=TRUE, rug=TRUE, legend=TRUE, ...) {

  .plotTss(x, y, counts=counts, ratio=ratio, fit=fit, expect=expect, tss=tss,
           threshold=threshold, rug=rug, legend=legend, ...)
}
)
