\name{TssRaw-class}

\docType{class}

\alias{TssRaw}
\alias{TssRaw-class}
\alias{Tss}
\alias{Tss,integer,integer-method}

\title{
  Class \code{TssRaw}
}

\description{
  TssRaw
}

\section{Slots}{
  \describe{
    \item{\code{data}:}{data}
    \item{\code{region}:}{region}
    \item{\code{chr}:}{chr}
    \item{\code{date}:}{date}
  }
}

\section{Methods}{
  \describe{
    \item{Tss:}{
      \code{signature(nReads="integer", start="integer"}
    }
  }
}

\examples{
x <- Tss(nReads=10:1, start=1:10, region=rep(1:2, 5))

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

x1 <- Tss(d1$nReads, d1$start)
x2 <- Tss(d2$nReads, d2$start)
x3 <- Tss(d3$nReads, d3$start)
x4 <- Tss(d4$nReads, d4$start)

}

\keyword{classes}
\keyword{methods}
