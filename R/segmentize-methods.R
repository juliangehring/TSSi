## segmentize ##
setGeneric("segmentize",
           function(counts, start, end=start, chr=rep(1L, length(start)),
                    region=rep(1L, length(start)), strand=rep("*", length(start)),
                    replicate=rep(1L, length(start)), annotation=NULL, ...)
           standardGeneric("segmentize")
           )

setMethod("segmentize",
          signature(counts="integer", start="integer"),
          function(counts, start, end=start, chr=rep(1L, length(start)),
                   region=rep(1L, length(start)), strand=rep("*", length(start)),
                   replicate=rep(1L, length(start)), annotation=NULL,
                   pattern="%1$s_%2$s_%3$s", ...) {

  ## check length of arguments
  if(!all(sapply(list(start, chr, region, strand, replicate), length) == length(counts)))
    warning("Input arguments do not have the same length.")
  
  ## order data
  ord <- order(chr, region, strand, start, end)
  region <- region[ord] ## remove?
  strand <- strand[ord] ## remove?
  chr <- chr[ord]
  y <- data.frame(start=start[ord], end=end[ord],
                  counts=counts[ord], replicate=replicate[ord])

  ## check for duplicate positions within the replicates
  if(any(diff(y$start) == 0 & diff(y$replicate) == 0))
    stop("Duplicated start positions within a replicate are not allowed.")
  
  ## TODO: check types

  ## find boundaries of segments
  segmentNames <- sprintf(pattern, chr, strand, region)
  rle <- rle(segmentNames)
  ind2 <- cumsum(rle$lengths)
  ind1 <- c(1L, ind2[-length(ind2)]+1L)

  ## break data into list for all segments, name list elements
  reads <- lapply(1:length(ind1), .breakInSegments, y=y, i1=ind1, i2=ind2)
  names(reads) <- segmentNames[ind1]

  ## segments
  nPos <- sapply(reads, nrow)
  nCounts <- sapply(reads, .colFun, "counts", sum)
  segments <- data.frame(chr=factor(chr[ind1]), strand=factor(strand[ind1]),
                         region=region[ind1], nPos=nPos, nCounts=nCounts,
                         start=y$start[ind1], end=y$end[ind2],
                         row.names=names(reads))

  ## create TssData object
  res <- new("TssData",
             reads=reads, segments=segments, annotation=annotation, timestamp=Sys.time())

  return(res)
}
)
