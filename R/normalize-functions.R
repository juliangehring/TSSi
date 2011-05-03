## normalize ##
.normalize <- function(x, fun=mean, offset=10, basal=1e-4, ratio,
                       regpara=c(1, 1), fit=FALSE) {

  ## extract data
  start <- x$start
  end <- x$end
  nReads <- x$nReads
  
  ## average over replicates if any present
  dup <- duplicated(start)
  if(any(dup)) {
    ## find regions of duplicates
    rle <- rle(start)
    ind2 <- cumsum(rle$lengths)
    ind1 <- c(1L, ind2[-length(ind2)]+1L)
    mReads <- sapply(1:length(ind1), .normDup, y=nReads, i1=ind1, i2=ind2, fun=fun)
    nRep <- rle$lengths
  } else {
    mReads <- nReads
    nRep <- rep(1L, length(mReads))
  }

  ## add basal offsets
  m <- length(mReads)
  indEst <- (1L:m) + offset
  longReads <- rep(basal, m+2*offset)
  longReads[indEst] <- mReads

  ## Poisson ratio
  lambda0 <- .initialFun(longReads, ratio, basal)[indEst]
  lambda <- rep(lambda0, nRep)

  ## store results from Poisson ratio
  res <- data.frame(start=start, end=end, nReads=nReads, lambda=lambda)

  if(fit == TRUE) {
    ## Poisson fit
    n <- length(lambda)
    lower <- rep(basal, n)
    rOpt <- optim(fn=.assess, gr=.assessGrad, par=lambda, method="L-BFGS-B",
                  lower=lower, control=list(trace=0, maxit=500), ## CHCK: maxit
                  nReads=nReads, regpara=regpara, basal=basal, nRep=nRep) ## TODO: arg names
    rTrust <- bobyqa(fn=.assess, par=lambda, lower=lower,
                     control=list(iprint=0, maxfun=10*n^2), ## CHCK: maxfun
                     nReads=nReads, regpara=regpara, basal=basal, nRep=NULL) ## TODO: arg names
    par <- if(rTrust$fval > rOpt$value) rOpt$par else rTrust$par
    optim <- if(rTrust$fval > rOpt$value) 1L else 2L

    ## store results from fit
    res$pReads <- par
    res$optim <- rep(optim, n)
  }

  return(res)
}


## initialRatio ##
.initialRatio <- function(nReads, regpara, basal) {

  lopt <- rep(NA, length(nReads))
  for(i in 1:length(nReads)) {
    lopt[i] <- optimize(f=.assess, nReads=nReads[i], regpara=regpara, basal=basal, nRep=NULL,
                        interval=c(0, nReads[i]+1))$minimum
  }
  ratio <- lopt/nReads
  
  return(ratio)
}


## initialFun ##
.initialFun <- function(nReads, ratio, basal) {

  ## CHCK: alternative method
  x <- 1:length(ratio)
  #ind <- nReads %in% x
  lambda0 <- nReads
  #lambda0[ind] <- ratio[nReads[ind]]
  #if(any(!ind))
  #  lambda0 <- approxExtrap(x, ratio, nReads[!ind])$y * nReads[!ind]
  #lambda0[lambda0 < 1] <- basal

  lambda0 <- approxExtrap(x, ratio, nReads)$y * nReads
  lambda0[lambda0 < 1] <- basal
  
  return(lambda0)
}


## colFun ##
.colFun <- function(x, col, fun) {
  
  res <- fun(x[ ,col])

  return(res)
}


## normDup ##
.normDup <- function(i, y, i1, i2, fun) {

  res <- fun(y[i1[i]:i2[i]])

  return(res)
}
