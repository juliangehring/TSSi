## identifyCore ##
.identifyCore <- function(x, basal, exppara, threshold, fun, cumulative) {

  ## extract data
  pos <- x$start
  fn <- counts <- x$ratio

  if(any(counts >= threshold)) {
    indTss <- NULL
    for(i in 1:length(counts)) {
      indTss[i] <- which.max(fn)
      cumBg <- .cumulativeReads(pos, counts, indTss, basal, exppara)
      dif <- fun(counts, cumBg$expect, indTss, basal, exppara) ## TODO assign by name?
      fn <- dif$delta
      fn[indTss] <- -Inf
      if(all(fn < threshold))
        break
    }

    ## sum up reads for each tss
    if(cumulative)
      yCounts <- diff(c(0, cumsum(dif$delta)[cumBg$indEnd])) ## useful?
    else
      yCounts <- dif$delta[indTss]
    
    tss <- data.frame(pos=pos[indTss], counts=yCounts)
    dif <- data.frame(delta=dif$delta, expect=dif$expect)
  } else {
    tss <- data.frame(pos=NULL, delta=NULL)
    dif <- data.frame(delta=NULL, expect=NULL)
  }
  res <- list(tss=tss, dif=dif)

  return(res)
}


## cumulativeReads ##
.cumulativeReads <- function(pos, expect, indTss, basal, exppara) {

  posTss <- pos[indTss]
  nPos <- length(pos)
  nTss <- length(posTss)

  ## calculate distance to each tss
  d <- matrix(pos, nTss, nPos, byrow=TRUE) - posTss
  da <- abs(d)

  ## find closest tss, choose parameter
  group <- apply(da, 2, which.min)
  idGroup <- group + seq(0L, by=nTss, length.out=nPos)
  ip <- ifelse(d[idGroup], 1L, 2L)

  ## calculate weights
  weight <- .exppdf(da[idGroup], exppara[ip]) / .exppdf(1, exppara[ip])
  weight[indTss] <- 1

  ## find indices for each group
  indEnd <- cumsum(rle(group)$lengths)

  ## compute cumulative weights for each tss, set everything else to basal
  cum <- diff(c(0, cumsum(weight*expect)[indEnd]))
  expect <- rep(basal, nPos)
  expect[indTss] <- cum

  return(list(expect=expect, indEnd=indEnd))
}


## exppdf ##
.exppdf <- function(x, mu) {

  res <- 1/mu * exp(-x/mu)

  return(res)
}
