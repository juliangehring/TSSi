## TssData ##
setGeneric("TssData",
           function(nReads, start, end=start, chr=rep(1L, length(start)),
                    region=rep(1L, length(start)), strand=rep(1L, length(start)),
                    replicate=rep(1L, length(start)), ...)
           standardGeneric("TssData")
           )

setMethod("TssData",
          signature(nReads="integer", start="integer"),
          function(nReads, start, end=start, chr=rep(1L, length(start)),
                   region=rep(1L, length(start)), strand=rep(1L, length(start)),
                   replicate=rep(1L, length(start)), ...) {
            ## TODO: class int for replicate

  ## check length of arguments
  if(!all(sapply(list(start, chr, region, strand, replicate), length) == length(nReads)))
    warning("Input arguments do not have the same length.")
  
  ## order data
  ord <- order(chr, region, start, end)
  region <- region[ord]
  chr <- factor(chr[ord])
  y <- data.frame(start=start[ord], end=end[ord], strand=factor(strand[ord]),
                  nReads=nReads[ord], replicate=replicate[ord])

  ## TODO: check types

  ## find boundaries of regions
  rle <- rle(paste(region, chr, sep=""))
  ind2 <- cumsum(rle$lengths)
  ind1 <- c(1L, ind2[-length(ind2)]+1)

  ## break data into list for all regions, name list elements
  data <- lapply(1:length(ind1), function(i, y, i1, i2) y[i1[i]:i2[i], ], y, ind1, ind2) ## TODO define externally
  regionNames <- as.character(region[ind1])
  names(data) <- regionNames

  res <- new("TssData",
             data=data, region=regionNames, chr=chr, date=date())

  return(res)
}
)
