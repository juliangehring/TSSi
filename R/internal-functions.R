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
