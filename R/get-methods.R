## get methods ##

## start ##
setGeneric("start",
           function(obj, ind)
           standardGeneric("start")
           )

setMethod("start",
          signature(obj="TssData"),
          function(obj, ind) {

  res <- .getSlotIndex(obj, ind, "reads", "start")

  return(res)
}
)


## end ##
setGeneric("end",
           function(obj, ind)
           standardGeneric("end")
           )

setMethod("end",
          signature(obj="TssData"),
          function(obj, ind) {

  res <- .getSlotIndex(obj, ind, "reads", "end")
  
  return(res)
}
)


## counts ##
setGeneric("counts",
           function(obj, ind)
           standardGeneric("counts")
           )

setMethod("counts",
          signature(obj="TssData"),
          function(obj, ind) {
            
  res <- .getSlotIndex(obj, ind, "reads", "counts")

  return(res)
}
)


## ratio ##
setGeneric("ratio",
           function(obj, ind)
           standardGeneric("ratio")
           )

setMethod("ratio",
          signature(obj="TssNorm"),
          function(obj, ind) {
            
  res <- .getSlotIndex(obj, ind, "reads", "ratio")

  return(res)
}
)


## fit ##
setGeneric("fit",
           function(obj, ind)
           standardGeneric("fit")
           )

setMethod("fit",
          signature(obj="TssNorm"),
          function(obj, ind) {
   
  res <- .getSlotIndex(obj, ind, "reads", "fit")

  return(res)
}
)


## delta ##
setGeneric("delta",
           function(obj, ind)
           standardGeneric("delta")
           )

setMethod("delta",
          signature(obj="TssResult"),
          function(obj, ind) {
   
  res <- .getSlotIndex(obj, ind, "reads", "delta")

  return(res)
}
)


## expect ##
setGeneric("expect",
           function(obj, ind)
           standardGeneric("expect")
           )

setMethod("expect",
          signature(obj="TssResult"),
          function(obj, ind) {
   
  res <- .getSlotIndex(obj, ind, "reads", "expect")

  return(res)
}
)


## reads ##
setGeneric("reads",
           function(obj, ind)
           standardGeneric("reads")
           )

setMethod("reads",
          signature(obj="TssData"),
          function(obj, ind) {

  res <- .getSlotIndex(obj, ind, "reads")

  return(res)
}
)


## regions ##
setGeneric("regions",
           function(obj, row, column, ...)
           standardGeneric("regions")
           )

setMethod("regions",
          signature(obj="TssData"),
          function(obj, row, column, drop=FALSE) {

  res <- obj@regions[row, column, drop]

  return(res)
}
)


## timestamp ##
setGeneric("timestamp",
           function(obj)
           standardGeneric("timestamp")
           )

setMethod("timestamp",
          signature(obj="TssData"),
          function(obj) {

  res <- obj@timestamp

  return(res)
}
)


## annotation ##
setGeneric("annotation",
           function(obj)
           standardGeneric("annotation")
           )

setMethod("annotation",
          signature(obj="TssData"),
          function(obj) {

  res <- obj@annotation

  return(res)
}
)


## parameters ##
setGeneric("parameters",
           function(obj, ind)
           standardGeneric("parameters")
           )

setMethod("parameters",
          signature(obj="TssNorm"),
          function(obj, ind) {

  res <- .getSlotIndex(obj, ind, "parameters")

  return(res)
}
)


## tss ##
setGeneric("tss",
           function(obj, ind)
           standardGeneric("tss")
           )

setMethod("tss",
          signature(obj="TssResult"),
          function(obj, ind) {

  res <- .getSlotIndex(obj, ind, "tss")

  return(res)
}
)
