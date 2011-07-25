## get methods ##

## start ##
setMethod("start",
          signature(x="TssData"),
          function(x, ind) {

  res <- .getSlotIndex(x, ind, "reads", "start")

  return(res)
}
)


## end ##
setMethod("end",
          signature(x="TssData"),
          function(x, ind) {

  res <- .getSlotIndex(x, ind, "reads", "end")
  
  return(res)
}
)


## counts ##
setGeneric("counts",
           function(x, ind)
           standardGeneric("counts")
           )

setMethod("counts",
          signature(x="TssData"),
          function(x, ind) {
            
  res <- .getSlotIndex(x, ind, "reads", "counts")

  return(res)
}
)


## ratio ##
setGeneric("ratio",
           function(x, ind)
           standardGeneric("ratio")
           )

setMethod("ratio",
          signature(x="TssNorm"),
          function(x, ind) {
            
  res <- .getSlotIndex(x, ind, "reads", "ratio")

  return(res)
}
)


## fit ##
setGeneric("fit",
           function(x, ind)
           standardGeneric("fit")
           )

setMethod("fit",
          signature(x="TssNorm"),
          function(x, ind) {
   
  res <- .getSlotIndex(x, ind, "reads", "fit")

  return(res)
}
)


## delta ##
setGeneric("delta",
           function(x, ind)
           standardGeneric("delta")
           )

setMethod("delta",
          signature(x="TssResult"),
          function(x, ind) {
   
  res <- .getSlotIndex(x, ind, "reads", "delta")

  return(res)
}
)


## expect ##
setGeneric("expect",
           function(x, ind)
           standardGeneric("expect")
           )

setMethod("expect",
          signature(x="TssResult"),
          function(x, ind) {
   
  res <- .getSlotIndex(x, ind, "reads", "expect")

  return(res)
}
)


## reads ##
setGeneric("reads",
           function(x, ind)
           standardGeneric("reads")
           )

setMethod("reads",
          signature(x="TssData"),
          function(x, ind) {

  res <- .getSlotIndex(x, ind, "reads")

  return(res)
}
)


## segments ##
setGeneric("segments",
           function(x, row, column, ...)
           standardGeneric("segments")
           )

setMethod("segments",
          signature(x="TssData"),
          function(x, row, column, drop=FALSE) {

  res <- x@segments[row, column, drop]

  return(res)
}
)


## timestamp ##
setGeneric("timestamp",
           function(x)
           standardGeneric("timestamp")
           )

setMethod("timestamp",
          signature(x="TssData"),
          function(x) {

  res <- x@timestamp

  return(res)
}
)


## annotation ##
setGeneric("annotation",
           function(x)
           standardGeneric("annotation")
           )

setMethod("annotation",
          signature(x="TssData"),
          function(x) {

  res <- x@annotation

  return(res)
}
)


## parameters ##
setGeneric("parameters",
           function(x, ind)
           standardGeneric("parameters")
           )

setMethod("parameters",
          signature(x="TssNorm"),
          function(x, ind) {

  res <- .getSlotIndex(x, ind, "parameters")

  return(res)
}
)


## tss ##
setGeneric("tss",
           function(x, ind)
           standardGeneric("tss")
           )

setMethod("tss",
          signature(x="TssResult"),
          function(x, ind) {

  res <- .getSlotIndex(x, ind, "tss")

  return(res)
}
)


## [ ##
setMethod("[",
          signature(x="TssData", i="ANY"),
          function(x, i) {

  res <- new("TssData",
             x, reads=reads(x)[i], segments=segments(x, i))

  return(res)
}
)

setMethod("[",
          signature(x="TssNorm", i="ANY"),
          function(x, i) {

  res <- new("TssNorm",
             x, reads=reads(x)[i], segments=segments(x, i))

  return(res)
}
)

setMethod("[",
          signature(x="TssResult", i="ANY"),
          function(x, i) {

  res <- new("TssResult",
             x, reads=reads(x)[i], segments=segments(x, i), tss=tss(x, )[i])

  return(res)
}
)
