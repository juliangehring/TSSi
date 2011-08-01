## getSlotIndex ##
.getSlotIndex <- function(x, ind, slotName, fieldName) {

  if(!missing(fieldName)) {
    if(!missing(ind))
      res <- slot(x, slotName)[[ind]][[fieldName]]
    else
      res <- lapply(slot(x, slotName), '[[', fieldName)
  } else {
    if(!missing(ind))
      res <- slot(x, slotName)[[ind]]
    else
      res <- slot(x, slotName)
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
.showSegment <- function(x) {

  segments <- segments(x)
  indent <- "   "

  cat(sprintf("** %s **\n", "Segments"))
  cat(.showSome(rownames(segments), "Segments", indent))
  cat(.showSome(unique(segments$chr), "Chromosomes", indent))
  cat(.showSome(unique(segments$strand), "Strands", indent))
  cat(.showSome(unique(segments$region), "Regions", indent))
  cat(.showSome(segments$nCounts, "nCounts", indent))
  if(as.character(class(x)) == "TssResult")
    cat(.showSome(segments$nTss, "nTSS", indent))
  cat("\n")

}


## showClassInfo ##
.showClassInfo <- function(x) {

  className <- as.character(class(x))
  msg <- switch(className,
                TssData="Data imported",
                TssNorm="Data normalized",
                TssResult="TSS in data identified")
  
  cat(sprintf("* Object of class '%s' *\n", className))
  cat(sprintf("  %s\n\n", msg))

}


## showTimestamp ##
.showTimestamp <- function(x) {

  timestamp <- as.character(timestamp(x))

  cat(sprintf("** %s **\n", "Timestamp"))
  cat(sprintf("   %s\n\n", timestamp))

}


## showParameters ##
.showParameters <- function(x) {

  pars <- parameters(x)
  
  cat(sprintf("** %s **\n", "Parameters"))
  cat(sprintf("   %s: %s\n", names(pars), pars), sep="")
  cat("\n")

}
