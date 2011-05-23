## names ##
setMethod("names",
          signature(x="TssData"),
          function(x) {

  res <- rownames(regions(x))

  return(res)
}
)
