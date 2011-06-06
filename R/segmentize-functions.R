## breakInSegments ##
.breakInSegments <- function(i, y, i1, i2) {

  res <- y[i1[i]:i2[i], ]

  return(res)
}
