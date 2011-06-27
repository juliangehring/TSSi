## breakInSegments ##
.breakInSegments <- function(i, y, i1, i2) {

  res <- y[i1[i]:i2[i], ]

  return(res)
}


## checkSegmentize ##
.checkSegmentize <- function(counts, start, end, chr, region, strand, replicate, annotation, pattern) {

  ## check length
  if(!all(sapply(list(start, chr, region, strand, replicate), length) == length(counts)))
    warning("Input arguments must have the same length.")

  ## 'pattern'
  .checkVariable(pattern, class="character")

}
