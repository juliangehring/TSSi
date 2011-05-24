\name{segmentize-methods}

\docType{methods}

\alias{segmentize}
\alias{segmentize,integer,integer-method}

\title{
  Segmentize methods
}

\description{
  Import sequence read data into the \pkg{mTSS} package.
}

\usage{
segmentize(counts, start, end=start, chr=rep(1L, length(start)),
region=rep(1L, length(start)), strand=rep(1L, length(start)),
replicate=rep(1L, length(start)), annotation=NULL, ...)
}

\arguments{
  \item{counts}{Integer vector with the number of reads for each
    position in \code{start}.}
  \item{start}{Integer vector with the start position of each read.}
  \item{end}{Optional integer vector with the end position of each
    read.}
  \item{chr}{Optional vector with the chromosomal location of each
    read.}
  \item{region}{Optional vector with an assignment of each read to a
    seperate region, for example based on additional annotation.}
  \item{strand}{Optional vector with the strand location of each read.}
  \item{replicate}{Optional integer vector indicating the replicate each
    read was obtained from, in the case of data from multiple
    measurements.}
  \item{annotation}{Optional object containing metadata passed along it
    the analysis. This argument does not affect the analysis in any
    kind.}
  \item{...}{Optional arguments, currently not used.}
}

\section{Methods}{
  Import read data:
  \describe{
    \item{segmentize:}{
      \code{signature(nReads="integer", start="integer")}
    }{
      \code{plot(x, y, counts=TRUE, legend=TRUE, ...)}
    }
  }
}

\details{
  The \code{segmentize} method takes the raw data and breaks it into
  segments which will be analyzed seperately in the subsequent
  steps. Segments are defined in a way such that any has a unique
  combination of the input arguments \code{chr}, \code{region}, and
  \code{strand}. In case any of these is not supplied it is assumend
  that all reads belong to one chromosome, region, or strand,
  respectively. Usage of the \code{region} argument is beneficial if the
  location of potential TSS can be contrained below the level of
  chromosomes and strands.
}

\value{
  An object of class \code{TssData}.
}

\author{
  Maintainer: Julian Gehring <julian.gehring@fdm.uni-freiburg.de>
}

\examples{
x <- segmentize(counts=10:1, start=1:10, region=rep(1:2, 5),
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

x1 <- segmentize(d1$nReads, d1$start)
x2 <- segmentize(d2$nReads, d2$start)
x3 <- segmentize(d3$nReads, d3$start)
x4 <- segmentize(d4$nReads, d4$start)

x5 <- segmentize(counts=d4$nReads, start=d4$start, end=d4$start,
chr=rep(1:2, each=24), region=rep(1:4, each=12), replicate=rep(1:4, 12),
annotation=list("test data"), strand=rep(c("+", "-"), 24))


## load data
example(readData)

## import and segmentize data
attach(readData)
x <- segmentize(counts=counts, start=start, chr=chromosome,
region=region, strand=strand)
detach(readData)
}

\keyword{methods}
\keyword{IO}

