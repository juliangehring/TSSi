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


## showSome ##
.showSome <- function(x, name, indent="") {

  res <- sprintf("%s%s (%d): %s\n",
                 indent, name, length(x), paste(selectSome(x), collapse=", "))

  return(res)
}


## showSegment ##
.showSegment <- function(object) {

  segments <- segments(object)
  indent <- "   "

  cat(sprintf("** %s **\n", "Segments"))
  cat(.showSome(rownames(segments), "Segments", indent))
  cat(.showSome(unique(segments$chr), "Chromosomes", indent))
  cat(.showSome(unique(segments$strand), "Strands", indent))
  cat(.showSome(unique(segments$region), "Regions", indent))
  cat(.showSome(segments$nCounts, "nCounts", indent))
  if(as.character(class(object)) == "TssResult")
    cat(.showSome(segments$nTss, "nTSS", indent))
  cat("\n")

}


## showClassInfo ##
.showClassInfo <- function(object) {

  className <- as.character(class(object))
  msg <- switch(className,
                TssData="Data imported",
                TssNorm="Data normalized",
                TssResult="TSS in data identified")
  
  cat(sprintf("* Object of class '%s' *\n", className))
  cat(sprintf("  %s\n\n", msg))

}


## showTimestamp ##
.showTimestamp <- function(object) {

  timestamp <- as.character(timestamp(object))

  cat(sprintf("** %s **\n", "Timestamp"))
  cat(sprintf("   %s\n\n", timestamp))

}


## showParameters ##
.showParameters <- function(object) {

  pars <- parameters(object)
  
  cat(sprintf("** %s **\n", "Parameters"))
  cat(sprintf("   %s: %s\n", names(pars), pars), sep="")
  cat("\n")

}
