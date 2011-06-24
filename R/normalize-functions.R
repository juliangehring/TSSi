## normalize ##
.normalize <- function(x, fun=mean, offset=10L, basal=1e-4, initial,
                       lambda=c(1, 1), fit=FALSE, optimizer="all") {

  ## extract data
  start <- x$start
  end <- x$end
  counts <- x$counts
  
  ## average over replicates if any present
  dup <- duplicated(start)
  if(any(dup)) {
    ## find segments of duplicates
    rle <- rle(start)
    ind2 <- cumsum(rle$lengths)
    ind1 <- c(1L, ind2[-length(ind2)]+1L)
    mReads <- sapply(1:length(ind1), .normDup, y=counts, i1=ind1, i2=ind2, fun=fun)
    nRep <- rle$lengths
  } else {
    mReads <- counts
    nRep <- rep(1L, length(mReads))
  }

  ## add basal offsets
  m <- length(mReads)
  indEst <- (1L:m) + offset
  longReads <- rep(basal, m+2*offset)
  longReads[indEst] <- mReads

  ## Poisson ratio
  ratio <- rep(.initialFun(longReads, initial, basal)[indEst], nRep)

  ## store results from Poisson ratio
  res <- data.frame(start=start, end=end, counts=counts, ratio=ratio)

  if(fit == TRUE) {
    ## Poisson fit
    n <- length(ratio)
    lower <- rep(basal, n)
    
    rOpt <- if(optimizer %in% c("optim", "all"))
      optim(fn=.assess, gr=.assessGrad, par=ratio, method="L-BFGS-B",
            lower=lower, control=list(trace=0, maxit=500), ## CHCK: maxit
            counts=counts, lambda=lambda, basal=basal, nRep=nRep) ## TODO: arg names
    else
      list(value=Inf)
    
    rTrust <- if(optimizer %in% c("bobyqa", "all"))
      bobyqa(fn=.assess, par=ratio, lower=lower,
             control=list(iprint=0, maxfun=10*n^2), ## CHCK: maxfun
             counts=counts, lambda=lambda, basal=basal, nRep=NULL) ## TODO: arg names
    else
      list(fval=Inf)

    ## store best fit
    res$fit <- if(rTrust$fval > rOpt$value) rOpt$par else rTrust$par
  }

  return(res)
}


## initialRatio ##
.initialRatio <- function(counts, lambda, basal) {

  lopt <- rep(NA, length(counts))
  for(i in 1:length(counts)) {
    lopt[i] <- optimize(f=.assess, counts=counts[i], lambda=lambda, basal=basal, nRep=NULL,
                        interval=c(0, counts[i]+1))$minimum
  }
  initial <- lopt/counts
  
  return(initial)
}


## initialFun ##
.initialFun <- function(counts, initial, basal) {

  ## CHCK: alternative method
  x <- 1:length(initial)
  #ind <- counts %in% x
  ratio0 <- counts
  #ratio0[ind] <- ratio[counts[ind]]
  #if(any(!ind))
  #  ratio0 <- approxExtrap(x, ratio, counts[!ind])$y * counts[!ind]
  #ratio0[ratio0 < 1] <- basal

  ratio0 <- approxExtrap(x, initial, counts)$y * counts
  ratio0[ratio0 < 1] <- basal
  
  return(ratio0)
}


## normDup ##
.normDup <- function(i, y, i1, i2, fun) {

  res <- fun(y[i1[i]:i2[i]])

  return(res)
}
