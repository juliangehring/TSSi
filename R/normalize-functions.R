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
    #.nRep <- function(x) length(unique(x))
    start <- start[ind1]
    end <- end[ind1]
    mReads <- sapply(1:length(ind1), .normDup, y=nReads, i1=ind1, i2=ind2, fun=fun)
    nRep <- rle$lengths
    #strand0 <- sapply(i=1:length(ind1), normDup, y=nReads0, i1=ind1, i2=ind2, fun=fun)
    #replicate0 <- sapply(i=1:length(ind1), normDup, y=nReads0, i1=ind1, i2=ind2, fun=fun)
  } else {
    mReads <- nReads
    nRep <- rep(1L, length(mReads))
  }
  nData <- length(mReads)

  ## add basal offsets
  indEst <- (1L:nData) + offset
  #pos <- c(start[1L]+(-offset:-1L), start, start[length(start)]+(1L:offset))
  longReads <- rep(basal, nData+2*offset)
  longReads[indEst] <- mReads

  ## Poisson ratio
  lambda0 <- .initialFun(longReads, ratio, basal)[indEst]

  ## store results from Poisson ratio
  res <- data.frame(start=start, end=end, mReads=mReads, lambda0=lambda0, nRep=nRep)

  if(fit == TRUE) {
    ## Poisson fit
    rOpt <- optim(fn=.assess, gr=.assessGrad, par=lambda0, method="L-BFGS-B",
                  lower=rep(basal, nData), control=list(trace=0, maxit=500), ## CHCK: maxit
                  nReads=mReads, regpara=regpara, basal=basal, nRep=nRep) ## TODO: arg names
    rTrust <- bobyqa(fn=.assess, par=lambda0, lower=rep(basal, nData),
                     upper=Inf, control=list(iprint=0, maxfun=10*nData^2), ## CHCK: maxfun
                     nReads=mReads, regpara=regpara, basal=basal, nRep=NULL) ## TODO: arg names
    par <- if(rTrust$fval > rOpt$value) rOpt$par else rTrust$par
    optim <- if(rTrust$fval > rOpt$value) 1L else 2L

    ## store results from fit
    res$pReads <- par
    res$optim <- rep(optim, nData)
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
