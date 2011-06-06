## readsAsRangedData
setGeneric("readsAsRangedData",
           function(x, ...)
           standardGeneric("readsAsRangedData")
           )

setMethod("readsAsRangedData",
          signature(x="TssData"),
          function(x) {

  segments <- segments(x)
  n <- segments$nPos

  reads <- rbind.fill(reads(x))
  reads$chr <- Rle(rep(segments$chr, n))
  reads$strand <- Rle(rep(segments$strand, n))
  reads$space <- rep(segments$region, n)

  res <- as(reads, "RangedData")
  names(res) <- names(x)

  return(res)
})


## tssAsRangedData
setGeneric("tssAsRangedData",
           function(x)
           standardGeneric("tssAsRangedData")
           )

setMethod("tssAsRangedData",
          signature(x="TssResult"),
          function(x) {

  tss <- tss(x)
  n <- sapply(tss, nrow)
  segments <- segments(x)

  tss <- rbind.fill(tss)

  ir <- IRanges(start=tss$pos, end=tss$pos)
  res <- RangedData(ranges=ir,
                    space=rep(segments$region, n),
                    chr=Rle(rep(segments$chr, n)),
                    strand=Rle(rep(segments$strand, n))
                    )
  names(res) <- names(x)
  
  return(res)
})


## segmentsAsRangedData
setGeneric("segmentsAsRangedData",
           function(x)
           standardGeneric("segmentsAsRangedData")
           )

setMethod("segmentsAsRangedData",
          signature(x="TssData"),
          function(x) {

  segments <- segments(x)
  segments$space <- names(x)

  res <- as(segments, "RangedData")
  names(res) <- names(x)

  return(res)
})
