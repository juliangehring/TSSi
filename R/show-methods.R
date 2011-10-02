## show ##
setMethod("show",
          signature(object="TssData"),
          function(object) {
  
  .showClassInfo(object)
  .showSegment(object)

}
)


## show ##
setMethod("show",
          signature(object="TssNorm"),
          function(object) {
  
  .showClassInfo(object)
  .showSegment(object)
  .showParameters(object)

}
)


## show ##
setMethod("show",
          signature(object="TssResult"),
          function(object) {
  
  .showClassInfo(object)
  .showSegment(object)
  .showParameters(object)

}
)
