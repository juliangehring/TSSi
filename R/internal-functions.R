## getSlotIndex ##
.getSlotIndex <- function(obj, ind, slotName, fieldName) {

  if(!missing(fieldName)) {
    if(!missing(ind))
      res <- slot(obj, slotName)[[ind]][[fieldName]]
    else
      res <- lapply(slot(obj, slotName), '[[', fieldName)
  } else {
    if(!missing(ind))
      res <- slot(obj, slotName)[[ind]]
    else
      res <- slot(obj, slotName)
  }
  return(res)
}


.getArgs <- function(name, first=NULL, last=NULL, ...) {

  ind <- which(names(...) %in% name)[1] ## [[]] allows only one element for indexing
  middle <- if(length(ind) != 0) ...[[ind]] else NULL
  args <- c(first, middle, last)
  args <- args[!duplicated(names(args))]
  
  return(args)
}


## regionize ##
.breakInSegments <- function(i, y, i1, i2) {

  res <- y[i1[i]:i2[i], ]

  return(res)
}


## useMulticore ##
.useMulticore <- function(multicore=TRUE) {

  res <- multicore && "multicore" %in% .packages() && any("mclapply" %in% objects("package:multicore"))

  return(res)
}
