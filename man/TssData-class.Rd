\name{TssData-class}

\docType{class}

\alias{TssData}
\alias{TssData-class}
\alias{TssData,integer,integer-method}

\title{
  Class \code{TssData}
}

\description{
  TssData
}

\section{Slots}{
  \describe{
    \item{\code{reads}:}{reads}
    \item{\code{regions}:}{regions}
    \item{\code{annotation}:}{annotation}
    \item{\code{timestamp}:}{timestamp}
  }
}

\section{Methods}{
  \describe{
    \item{TssData:}{
      \code{signature(nReads="integer", start="integer"}
    }
  }
}

\examples{
x <- TssData(counts=10:1, start=1:10, region=rep(1:2, 5),
annotation="test data")

tssPeak <- function(pos=1:100, posTss=c(32, 64), mu=c(2, 2), scale=c(1e3, 1e3)) {

  if(!all.equal(length(posTss), length(mu), length(scale)))
    stop("args have not same length")

  n <- length(posTss)
  z <- rep(0, length(pos))
  for(i in 1:n) {
    y <- dexp(abs(pos-posTss[i]), 1/mu[i])
    z <- z + floor(y*scale[i])
  }
  
  ind <- z > 0
  start <- pos[ind]
  nReads <- as.integer(z[ind])

  res <- list(start=start, nReads=nReads, posTss=posTss, mu=mu, scale=scale)

  return(res)
}

d1 <- tssPeak(posTss=32, mu=2, scale=1e3)
d2 <- tssPeak()
d3 <- tssPeak(posTss=c(32, 42))
d4 <- tssPeak(scale=c(1e3, 5e2))

x1 <- TssData(d1$nReads, d1$start)
x2 <- TssData(d2$nReads, d2$start)
x3 <- TssData(d3$nReads, d3$start)
x4 <- TssData(d4$nReads, d4$start)

x5 <- TssData(counts=d4$nReads, start=d4$start, end=d4$start,
chr=rep(1:2, each=24), region=rep(1:4, each=12), replicate=rep(1:4, 12),
annotation=list("test data"), strand=rep(c("+", "-"), 24))
}

\keyword{classes}
\keyword{methods}
