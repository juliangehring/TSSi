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


## checkVariable ##
.checkVariable <- function(x, class, type, length, range, value, call.=FALSE) {

  name <- deparse(substitute(x))

  ## check class
  if(!missing(class)) {
    if(class == "integer") {
      if(any(x %% 1 != 0))
        stop(sprintf("'%s' %s.", name, "must be an integer"), call.=call.)
    } else {
      if(class != class(x))
        stop(sprintf("'%s' %s '%s'.", name, "must be of class", class(x)), call.=call.)
    }
  }

  ## check type
  if(!missing(type)) {
    if(typeof(x) != type)
      stop(sprintf("'%s' %s %s.", name, "must be of type", typeof(x)), call.=call.)
  }

  ## check length
  if(!missing(length)) {
    if(length(x) != length)
      stop(sprintf("'%s' %s %g.", name, "must be of length", length(x)), call.=call.)
  }

  ## check range
  if(!missing(range)) {
    if(min(x) < min(range) || max(x) > max(range))
      stop(sprintf("'%s' %s [%g,%g].", name, "must be in the range", min(x), max(x)), call.=call.)
  }

  ## check value
  if(!missing(value)) {
    if(!all.equal(x, value))
      stop(sprintf("'%s' %s.", name, "must be equal the reference value"), call.=call.)
  }

}


## grepStrand ##
.grepStrand <- function(x, strand="\\-") {

  gpattern <- sprintf(parameters(x, "pattern"), ".*", strand, ".*")
  names <- names(x)
  res <- grepl(gpattern, names)
  names(res) <- names

  return(res)
}


## revertPars ##
.revertPars <- function(i, strand, res) {

  if(strand[i]) res <- rev(res)
  
  return(res)
}
