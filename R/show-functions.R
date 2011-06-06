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
