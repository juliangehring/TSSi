## useMulticore ##
.useMulticore <- function(multicore=TRUE) {

  res <- multicore && "multicore" %in% .packages() && any("mclapply" %in% objects("package:multicore"))

  return(res)
}


## colFun ##
.colFun <- function(x, col, fun) {
  
  res <- fun(x[ ,col])

  return(res)
}
