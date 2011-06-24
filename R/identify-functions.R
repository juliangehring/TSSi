## identifyCore ##
.identifyCore <- function(x, basal, tau, threshold, fun, readCol, neighbor) {

  ## extract data
  pos <- x$start
  fn <- counts <- x[[readCol]]
  n <- length(counts)

  if(any(counts >= threshold)) {
    indTss <- logical(n)
    for(i in 1:n) {
      indTss[fn == max(fn)] <- TRUE
      bg <- .cumulativeReads(pos, counts, indTss, basal, tau, neighbor)
      dif <- fun(counts, bg, indTss, pos, basal, tau) ## TODO assign by name?
      fn <- dif$delta
      fn[indTss] <- -Inf
      if(all(fn < threshold))
        break
    }
    tss <- data.frame(pos=pos[indTss], reads=dif$delta[indTss])
    dif <- data.frame(delta=dif$delta, expect=dif$expect)
  } else {
    tss <- data.frame(pos=NULL, delta=NULL)
    dif <- data.frame(expect=rep(basal, n))
  }
  res <- list(tss=tss, dif=dif)

  return(res)
}


## cumulativeReads ##
.cumulativeReads <- function(pos, expect, indTss, basal, tau, neighbor) {

  posTss <- pos[indTss]
  nPos <- length(pos)
  nTss <- length(posTss)

  ## calculate distance to each tss
  d <- matrix(pos, nTss, nPos, byrow=TRUE) - posTss
  da <- abs(d)

  ## find closest tss, choose parameter
  group <- apply(da, 2, which.min)
  idGroup <- group + seq(0L, by=nTss, length.out=nPos)
  ip <- ifelse(d[idGroup] < 0, 1L, 2L)

  ## calculate weights
  weight <- .exppdf(da[idGroup], tau[ip]) / .exppdf(0, tau[ip])
  weight[indTss] <- 1  ## TODO needed?

  if(neighbor) {
    ## find indices for each group
    indEnd <- cumsum(rle(group)$lengths)
    
    ## compute cumulative weights for each tss, set everything else to basal
    cum <- diff(c(0, cumsum(weight*expect)[indEnd]))
    expect <- rep(basal, nPos)
    expect[indTss] <- cum
  }
  else {
    expect <- weight*expect
  }

  return(expect)
}


## exppdf ##
.exppdf <- function(x, mu) {

  res <- 1/mu * exp(-x/mu)

  return(res)
}
