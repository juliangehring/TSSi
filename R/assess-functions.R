## assess ##
.assess <- function(ratio, counts, lambda, basal, nRep) {

  ass <- c(2*.nReadsLoglik(counts, ratio),
           lambda[1]*.assessAbs(ratio, basal),
           lambda[2]*.assessSteps(ratio, basal))

  res <- sum(ass)

  return(res)
}


## assessAbs ##
.assessAbs <- function(ratio, basal) {

  out <- .C("assessAbs", PACKAGE="TSSi",
            ratio=as.double(ratio), basal=as.double(basal),
            nin=as.integer(length(ratio)),
            ass=double(1L))
  res <- out$ass
  
  return(res)
}


## assessSteps ##
.assessSteps <- function(ratio, basal) {

  ratio2 <- c(basal, ratio, basal)

  out <- .C("assessSteps", PACKAGE="TSSi",
            ratio2=as.double(ratio2), len=as.integer(length(ratio2)),
            ass=double(1L))
  
  res <- out$ass
  
  return(res)
}


## nReadsLoglik ##
.nReadsLoglik <- function(x, ratio) {

  prob <- dpois(x, ratio)
  res <- -sum(log(prob[prob > 0]))

  return(res)
}


## assessGrad ##
.assessGrad <- function(ratio, counts, lambda, basal, nRep) {

  dass1 <- 2*.nReadsLoglikGrad(counts, ratio, nRep)
  dass2 <- lambda[1]*.assessAbsGrad(ratio, basal)
  dass3 <- lambda[2]*.assessStepsGrad(ratio, basal)
  
  res <- dass1 + dass2 + dass3

  return(res)
}


## assessAbsGrad ##
.assessAbsGrad <- function(ratio, basal) {

  out <- .C("assessAbsGrad", PACKAGE="TSSi",
            ratio=as.double(ratio), basal=as.double(basal),
            nin=as.integer(length(ratio)),
            dass=double(1L))
  
  res <- out$dass
  
  return(res)
}


## assessStepsGrad ##
.assessStepsGrad <- function(ratio, basal) {

  ratio2 <- c(basal, ratio, basal)
  len2 <- length(ratio2)

  out <- .C("assessStepsGrad", PACKAGE="TSSi",
            ratio2=as.double(ratio2), len2=as.integer(len2),
            dass=double(1L))
  
  res <- out$dass
  
  return(res)
}


## nReadsLoglikGrad ##
.nReadsLoglikGrad <- function(x, ratio, nRep) {

  res <- nRep - x/ratio

  return(res)
}


## assessRatio ##
.assessRatio <- function(ratio, counts, lambda, basal) {

    ass1 <- 2*.nReadsLoglik(counts, ratio)
    ass2 <- lambda[1]*.assessAbs(ratio, basal)
    ass <- ass1+ass2

    return(ass)
}

