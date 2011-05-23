## names ##
setMethod("names",
          signature(x="TssData"),
          function(x) {

  res <- rownames(segments(x))

  return(res)
}
)
