## show ##
setMethod("show",
          signature(object="TssData"),
          function(object) {
  
  .showClassInfo(object)
  .showSegment(object)
  .showTimestamp(object)

}
)


## show ##
setMethod("show",
          signature(object="TssNorm"),
          function(object) {
  
  .showClassInfo(object)
  .showSegment(object)
  .showParameters(object)
  .showTimestamp(object)

}
)


## show ##
setMethod("show",
          signature(object="TssResult"),
          function(object) {
  
  .showClassInfo(object)
  .showSegment(object)
  .showParameters(object)
  .showTimestamp(object)

}
)
