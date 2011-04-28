.assess <- function(lambda, nReads, regpara, basal, dograd=FALSE) {		

  ndat <- max(1, sum(lambda > 2*basal, na.rm=TRUE))	# da jedes lambda 1 Mal., Faktor 2 wg. num. ungenauigkeit.
  
  if(dograd) {
    tmp1 <- .assessLikelihood(lambda, nReads)

    ass1  <- 2*tmp1$ass/log(ndat+1)
    dass1 <- 2*tmp1$dassDlambda/log(ndat+1)

    tmp2 <- .assessAbs(lambda, basal)

    ass2 <- 2*regpara[1]*tmp2$ass
    dass2 <- 2*regpara[1]*tmp2$dassDlambda
    
    tmp3 <- .assessSteps(lambda, basal)

    ass3 <- 2*regpara[2]*tmp3$ass
    dass3<- 2*regpara[2]*tmp3$dassDlambda
    
    ass <- ass1+ass2+ass3
    dassDlambda <- dass1+dass2+dass3
    
    OUT <- list(ass=ass, dassDlambda=dassDlambda)
  } else {
    ass <- c(2*.assessLikelihood(lambda, nReads)$ass/log(ndat+1), ## moved here
             2*regpara[1]*.assessAbs(lambda, basal)$ass,
             2*regpara[2]*.assessSteps(lambda, basal)$ass)

    ass <- sum(ass)
    OUT <- ass
  }
  
  if(is.nan(ass) | abs(ass)==Inf)
    stop("if(is.nan(ass) | abs(ass)==Inf")

  return(OUT)		
}


.assessAbs <- function(lambda, basal) {
  
  ass <- sum(abs((lambda-basal)/sqrt(lambda+basal)))
  dassDlambda <- abs(1/(lambda+basal)^(1/2)-1/2*(lambda-basal)/(lambda+basal)^(3/2))
  OUT <- list(ass=ass, dassDlambda=dassDlambda)
    
  return(OUT)
}


ycAssessAbs <- function(lambda, basal) {

  out <- .C("cAssessAbs", PACKAGE="TSS",
            lambda=as.double(lambda), basal=as.double(basal),
            nin=as.integer(length(lambda)), ass=double(1), dass=double(length(lambda)))
  OUT <- list(ass=out$ass, dassDlambda=out$dass)
  
  return(OUT)
}


.assessGrad <- function(lambda, nReads, regpara, basal, dograd=TRUE) {
  
  OUT <- .assess(lambda, nReads, regpara, basal, dograd=TRUE)$dassDlambda
  
  return(OUT)
}


.assessLikelihood <- function(lambda, nreads, dograd=TRUE) {
  
  if(is.null(dim(nreads)))
    nreads <- matrix(nreads, nrow=length(lambda))
  
  loglik <- .NreadsLoglik(nreads, lambda) # negative log-likelihood
  
  if(dograd) {
    dloglikDlambda <- .NreadsLoglikGrad(nreads, lambda)
    OUT <- list(ass=loglik, dassDlambda=dloglikDlambda)
  }
  else
    OUT <- list(ass=loglik)
  
  return(OUT)		
}	


.assessRatio <- function(lambda, nreads, regpara, basal) {

    ndat <- length(nreads)
    ass1 <- 2*.assessLikelihood(lambda, nreads, dograd=FALSE)$ass/log(ndat+1)    # Poisson -2*loglik +
    ass2 <- 2*regpara[1]*.assessAbs(lambda, basal)$ass     # Regularisierung, lambda möglichst nah an basalen level
    ass <- ass1+ass2

    return(ass)
}


.assessSteps <- function(lambda, basal, dograd=TRUE) {		

  lambda2 <- c(basal, lambda, basal)  # the basal value is added on the left an right side to be able to penalize a "jump" at the beginning/end
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


.NreadsLoglik <- function(x, lambda) {

  prob <- dpois(x, lambda)
  prob[prob <= 0] <- .Machine$double.eps
  out <- sum(-log(prob), na.rm=TRUE)

  return(out)
}


.NreadsLoglikGrad <- function(x, lambda) {

  if(!is.null(dim(x))) {  # x is a matrix
    if(dim(x)[2] > 1) { # x has at least two columns, i.e. with replicates
      n <- rowSums(!is.na(x))
      dloglik_dlambda <- -(-n + rowSums(x, na.rm=TRUE)/lambda) # sum is over replicates
    }
    else # without replicates
      dloglik_dlambda <- -(-1 + x/lambda)
  } else { # x is a number or an array
    if(length(lambda == 1)) { # replicates, single position
      n <- sum(!is.na(x))
      dloglik_dlambda <- -(-n + sum(x,na.rm=TRUE)/lambda)
    }
    else {# no replicates, multiple positions
      n <- 1
      dloglik_dlambda <- -(-n + x/lambda)
    }
  }

  out <- dloglik_dlambda

  return(out)
}

