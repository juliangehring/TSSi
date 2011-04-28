## assess ##
.assess <- function(lambda, nReads, regpara, basal) {	## nRep needed?

  n <- max(1, sum(lambda > 2*basal, na.rm=TRUE)) ## NA possible?
  
  ass <- c(2*.nReadsLoglik(nReads, lambda)/log(n+1),
           2*regpara[1]*.assessAbs(lambda, basal),
           2*regpara[2]*.assessSteps(lambda, basal)$ass) ## different functions?
  
  res <- sum(ass)
  
  if(is.nan(res) | abs(res)==Inf) ## CHCK needed?
    stop("if(is.nan(ass) | abs(ass)==Inf")

  return(res)		
}


## assessGrad ##
.assessGrad <- function(lambda, nReads, regpara, basal, nRep) {

  n <- max(1, sum(lambda > 2*basal, na.rm=TRUE)) ## NA possible?

  ass1  <- 2*.nReadsLoglik(nReads, lambda)/log(n+1)
  dass1 <- 2*.nReadsLoglikGrad(nReads, lambda, nRep)/log(n+1)
  
  ass2 <- 2*regpara[1]*.assessAbs(lambda, basal)
  dass2 <- 2*regpara[1]*.assessAbsGrad(lambda, basal)
  
  tmp3 <- .assessSteps(lambda, basal)
  
  ass3 <- 2*regpara[2]*tmp3$ass
  dass3<- 2*regpara[2]*tmp3$dassDlambda
  
  ass <- ass1+ass2+ass3
  dassDlambda <- dass1+dass2+dass3
  
  OUT <- list(ass=ass, dassDlambda=dassDlambda)
  
  return(OUT)
}


## assessAbs ##
.assessAbs <- function(lambda, basal) {
  
  ass <- sum(abs((lambda-basal)/sqrt(lambda+basal)))
    
  return(ass)
}


## ycAssessAbs ##
ycAssessAbs <- function(lambda, basal) {

  out <- .C("cAssessAbs", PACKAGE="TSS",
            lambda=as.double(lambda), basal=as.double(basal),
            nin=as.integer(length(lambda)), ass=double(1), dass=double(length(lambda)))
  OUT <- list(ass=out$ass, dassDlambda=out$dass)
  
  return(OUT)
}


## assessAbsGrad ##
.assessAbsGrad <- function(lambda, basal) {
  
  dass <- abs(1/(lambda+basal)^(1/2)-1/2*(lambda-basal)/(lambda+basal)^(3/2))
    
  return(dass)
}


## assessRatio ##
.assessRatio <- function(lambda, nReads, regpara, basal) {

    ndat <- length(nReads)
    ass1 <- 2*.nReadsLoglik(nReads, lambda)/log(ndat+1)
    ass2 <- 2*regpara[1]*.assessAbs(lambda, basal)
    ass <- ass1+ass2

    return(ass)
}


## assessSteps ##
.assessSteps <- function(lambda, basal, dograd=TRUE) {		

  lambda2 <- c(basal, lambda, basal)
  len <- length(lambda2)

  fuerSprung <- diff(lambda2)/sqrt(lambda2[-len]+lambda2[-1])	# Diff zur dessen SE
  ass <- sum(abs(fuerSprung))	

  if(dograd) {
    x <- lambda2[1:(len-2)]
    y <- lambda2[3:len]
    dassLast <- 1/(x+lambda)^(1/2)-1/2*(lambda-x)/(x+lambda)^(3/2)		
    dassNext <- -1/(y+lambda)^(1/2)-1/2*(y-lambda)/(y+lambda)^(3/2)	
    dassDlambda <- sign(lambda-x)*dassLast + sign(y-lambda)*dassNext	
    OUT <- list(ass=ass, dassDlambda=dassDlambda)
  }
  else
    OUT = list(ass=ass)
  
  return(OUT)
}


## ycAssessSteps ##
ycAssessSteps <- function(lambda, basal, dograd=TRUE) {

  lambda2 <- c(basal, lambda, basal)
  len <- length(lambda2)

  out <- .C("cAssessSteps", PACKAGE="TSS",
            lambda2=as.double(lambda2), len=as.integer(len),
            dograd=as.integer(dograd), ass=double(1L), dass=double(len-2))

  if(dograd)
    OUT <- list(ass=out$ass, dassDlambda=out$dass)
  else
    OUT <- list(ass=out$ass)
  
  return(OUT)
}


## nReadsLoglik ##
.nReadsLoglik <- function(x, lambda) {

  prob <- dpois(x, lambda)
  prob[prob <= 0] <- .Machine$double.eps ## TODO check
  res <- -sum(log(prob), na.rm=TRUE) ## CHCK NA possible?

  return(res)
}


## nReadsLoglikGrad ##
.nReadsLoglikGrad <- function(x, lambda, nRep) {

  res <- nRep - x/lambda

  return(res)
}

