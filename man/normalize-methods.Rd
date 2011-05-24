\name{normalize-methods}

\docType{methods}

\alias{normalize}
\alias{normalize,TssData-method}

\title{
  Normalize methods
}

\description{
  Normalize sequence read data.
}

\usage{
normalize(obj, fun=mean, offset=10L, basal=1e-4, regpara=c(1, 1),
fit=FALSE, multicore=TRUE, ...)
}

\arguments{
  \item{obj}{Object of class \code{TssData} with raw data to normalize.} 
  \item{fun}{Function used to average over replicates (default: mean).}
  \item{offset}{Integer defining the number of bases add to the ends of
    each segment with \code{basal} reads rate.}
  \item{basal}{Numeric specifying the basal read rate.}
  \item{regpara}{Vector with two numerics specifying the regulation
    parameter for each side of the segment.}
  \item{fit}{Logical whether the fitting should be performed in addition
    to the estimation based on the Poisson ratios obtained from all
    reads.}
  \item{multicore}{Logical whether to use the \pkg{multicore} package to
    parallelize the fitting. Has only an effect if the package is
    available and loaded. For details, see the \sQuote{details}
    section.}
  \item{...}{Additional arguments passed for the \pkg{multicore} package
    if used.For details, see the \sQuote{details} section.}
}

\section{Methods}{
  Normalize read data:
  \describe{
    \item{normalize:}{
      \code{signature(obj="TssData")}
    }{
      \code{normalize(obj, fun=mean, offset=10L, basal=1e-4,
	regpara=c(1, 1), fit=FALSE, multicore=TRUE, ...)}
    }
  }
}

\details{
  The \code{normalize} method preprocesses the raw read data, based on
  eighter the Poisson ratios obtained from the information of all reads
  or an fitting of the distribution.

  In order to speed up the fitting step, the \code{mclapply} function of
  \pkg{multicore} can be used. For this, the \pkg{multicore} package has
  to be loaded manully before starting the computation, additional
  parameters are passed via the \code{...} argument, e.g. as
  \code{normalize(obj, mc.ncores=2)}. The \code{multicore} argument can
  further be used to temporarily disable the parallel estimation by
  setting it to \code{FALSE}.
}

\value{
  An object of class \code{TssNorm}.
}

\author{
  Maintainer: Julian Gehring <julian.gehring@fdm.uni-freiburg.de>
}

\examples{
example(segmentize)

## normalize data, w/o and w/ fitting
yRatio <- normalize(x)
yFit <- normalize(x, fit=TRUE)
yFit
}

\keyword{methods}
\keyword{models}

