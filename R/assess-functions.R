## assess ##
.assess <- function(lambda, counts, regpara, basal, nRep) {

  ass <- c(2*.nReadsLoglik(counts, lambda),
           regpara[1]*.assessAbs(lambda, basal),
           regpara[2]*.assessSteps(lambda, basal))

  res <- sum(ass)

  return(res)
}


## assessAbs ##
.assessAbs <- function(lambda, basal) {

  out <- .C("assessAbs", PACKAGE="TSSi",
            lambda=as.double(lambda), basal=as.double(basal),
            nin=as.integer(length(lambda)),
            ass=double(1L))
  res <- out$ass
  
  return(res)
}


## assessSteps ##
.assessSteps <- function(lambda, basal) {

  lambda2 <- c(basal, lambda, basal)

  out <- .C("assessSteps", PACKAGE="TSSi",
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

  dass1 <- 2*.nReadsLoglikGrad(counts, lambda, nRep)
  dass2 <- regpara[1]*.assessAbsGrad(lambda, basal)
  dass3 <- regpara[2]*.assessStepsGrad(lambda, basal)
  
  res <- dass1 + dass2 + dass3

  return(res)
}


## assessAbsGrad ##
.assessAbsGrad <- function(lambda, basal) {

  out <- .C("assessAbsGrad", PACKAGE="TSSi",
            lambda=as.double(lambda), basal=as.double(basal),
            nin=as.integer(length(lambda)),
            dass=double(1L))
  res <- out$dass
  
  return(res)
}


## assessStepsGrad ##
.assessStepsGrad <- function(lambda, basal) {

  lambda2 <- c(basal, lambda, basal)
  len2 <- length(lambda2)

  out <- .C("assessStepsGrad", PACKAGE="TSSi",
            lambda2=as.double(lambda2), len2=as.integer(len2),
            dass=double(1L))
  
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

    ass1 <- 2*.nReadsLoglik(counts, lambda)
    ass2 <- regpara[1]*.assessAbs(lambda, basal)
    ass <- ass1+ass2

    return(ass)
}

