## assess ##
.assess <- function(lambda, counts, regpara, basal, nRep) {

  n <- max(1, sum(lambda > 2*basal))
  
  ass <- c(2*.nReadsLoglik(counts, lambda)/log(n+1),
           2*regpara[1]*.assessAbs(lambda, basal),
           2*regpara[2]*.assessSteps(lambda, basal))

  res <- sum(ass)

  return(res)
}


## assessAbs ##
.assessAbs <- function(lambda, basal) {

  out <- .C("assessAbs", PACKAGE="mTSS",
            lambda=as.double(lambda), basal=as.double(basal),
            nin=as.integer(length(lambda)),
            ass=double(1L))
  res <- out$ass
  
  return(res)
}


## assessSteps ##
.assessSteps <- function(lambda, basal) {

  lambda2 <- c(basal, lambda, basal)

  out <- .C("assessSteps", PACKAGE="mTSS",
            lambda2=as.double(lambda2), len=as.integer(length(lambda2)),
            ass=double(1L))
  
  res <- out$ass
  
  return(res)
}


## nReadsLoglik ##
.nReadsLoglik <- function(x, lambda) {

  prob <- dpois(x, lambda)
  res <- -sum(log(prob[prob > 0]))

  return(res)
}


## assessGrad ##
.assessGrad <- function(lambda, counts, regpara, basal, nRep) {

  n <- max(1, sum(lambda > 2*basal))

  dass1 <- 2*.nReadsLoglikGrad(counts, lambda, nRep)/log(n+1)
  dass2 <- 2*regpara[1]*.assessAbsGrad(lambda, basal)
  dass3 <- 2*regpara[2]*.assessStepsGrad(lambda, basal)
  
  res <- dass1+dass2+dass3

  return(res)
}


## assessAbsGrad ##
.assessAbsGrad <- function(lambda, basal) {

  out <- .C("assessAbsGrad", PACKAGE="mTSS",
            lambda=as.double(lambda), basal=as.double(basal),
            nin=as.integer(length(lambda)),
            dass=double(length(lambda)))
  res <- out$dass
  
  return(res)
}


## assessStepsGrad ##
.assessStepsGrad <- function(lambda, basal) {

  lambda2 <- c(basal, lambda, basal)
  len2 <- length(lambda2)

  out <- .C("assessStepsGrad", PACKAGE="mTSS",
            lambda2=as.double(lambda2), len2=as.integer(len2),
            dass=double(len2-2L))
  
  res <- out$dass
  
  return(res)
}


## nReadsLoglikGrad ##
.nReadsLoglikGrad <- function(x, lambda, nRep) {

  res <- nRep - x/lambda

  return(res)
}


## assessRatio ##
.assessRatio <- function(lambda, counts, regpara, basal) {

    ndat <- length(counts)
    ass1 <- 2*.nReadsLoglik(counts, lambda)/log(ndat+1)
    ass2 <- 2*regpara[1]*.assessAbs(lambda, basal)
    ass <- ass1+ass2

    return(ass)
}

