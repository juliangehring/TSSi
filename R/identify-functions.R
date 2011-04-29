## identifyCore ##
.identifyCore <- function(x, basal, exppara, threshold, fun) {

  ## extract data
  pos <- x$start
  fn <- nReads <- x$nReads
  lennReads <- length(nReads)

  if(any(fn > threshold)) { ## > or >= ???
    indTss <- which.max(fn) ## always chooses first maximum

    for(i in 1:lennReads) {
      cumBg <- .cumulativeReads(pos, nReads, pos[indTss], basal, exppara) ## expect or nReads? changed or unchanged??

      dif <- fun(nReads, cumBg, indTss, basal, exppara)

      fn <- dif$delta
      fn[indTss] <- -Inf
      
      if(any(fn > threshold))
        indTss <- c(indTss, which.max(fn))
      else
        break
    }
  } else {
    #expect <- rep(basal, lennReads) ## needed?
    res <- list(posTss=NULL, yTss=NULL,  delta=NULL, expect=NULL)    
  }

  res <- list(posTss=pos[indTss], yTss=dif$delta[indTss], delta=dif$delta,
              expect=dif$expect)

  return(res)
}


## cumulativeReads ##
.cumulativeReads <- function(pos, expect, posTss, basal, exppara,
                             weight0=.exppdf(1, exppara)) {

  ## pos: nReads > basal and not a TSS
  ind0 <- which(expect > basal & !(pos %in% posTss))

  for(i in ind0) {
    ## find closest TSS
    indPos <- which.min(abs(posTss - pos[i]))
    nextPos <- posTss[indPos]

    ## compute weights according to side
    ip <- if(nextPos < pos[i]) 1L else 2L
    weight <- .exppdf(abs(nextPos - pos[i]), exppara[ip]) / weight0[ip]

    ## add reads to next TSS, set current pos to basal
    expect[indPos] <- expect[indPos] + expect[i]*weight
    expect[i] <- basal
  }

  return(expect)
}


## exppdf ##
.exppdf <- function(x, mu) {

  res <- 1/mu * exp(-x/mu)

  return(res)
}
