## show ##
setMethod("show",
          signature(object="TssData"),
          function(object) {
  
  .showClassInfo(object)
  .showRegion(object)
  .showTimestamp(object)

}
)


## show ##
setMethod("show",
          signature(object="TssNorm"),
          function(object) {
  
  .showClassInfo(object)
  .showRegion(object)
  .showParameters(object)
  .showTimestamp(object)

}
)


## show ##
setMethod("show",
          signature(object="TssResult"),
          function(object) {
  
  .showClassInfo(object)
  .showRegion(object)
  .showParameters(object)
  .showTimestamp(object)

}
)


## showSome ##
.showSome <- function(x, name, indent="") {

  res <- sprintf("%s%s (%d): %s\n",
                 indent, name, length(x), paste(selectSome(x), collapse=", "))

  return(res)
}


## showRegion ##
.showRegion <- function(object) {

  regions <- regions(object)
  indent <- "   "

  cat(sprintf("** %s **\n", "Regions"))
  cat(.showSome(rownames(regions), "Segments", indent))
  cat(.showSome(unique(regions$chr), "Chromosomes", indent))
  cat(.showSome(unique(regions$strand), "Strands", indent))
  cat(.showSome(unique(regions$region), "Regions", indent))
  cat(.showSome(regions$nCounts, "nCounts", indent))
  if(as.character(class(object)) == "TssResult")
    cat(.showSome(regions$nTss, "nTSS", indent))
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


## showAnnotation ##
.showAnnotation <- function(object) {
}


## showParameters ##
.showParameters <- function(object) {

  pars <- parameters(object)
  
  cat(sprintf("** %s **\n", "Parameters"))
  cat(sprintf("   %s: %s\n", names(pars), pars), sep="")
  cat("\n")

}
