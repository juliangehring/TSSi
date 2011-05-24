\name{idenfify-methods}

\docType{methods}

\alias{identify}
\alias{identify,TssNorm-method}

\title{
  Identify methods
}

\description{
  Identify TSS in sequence read data.
}

\usage{
identify(obj, threshold=1, exppara=c(20, 20), fun=subtractExpectation,
multicore=TRUE, ...)
}

\arguments{
  \item{obj}{Object of class \code{TssNorm} with normalized data.}
  \item{threshold}{Numeric with the minimal number of reads to be
    treated as a potential TSS.}
  \item{exppara}{Vector with two numerics specifying the
    \eqn{\sQuote{mu}}{\mu} parameter of the exponential function for
    each side of the segment.} 
  \item{fun}{Function to calculate the expection for each TSS.}
  \item{multicore}{Logical whether to use the \pkg{multicore} package to
    parallelize the fitting. Has only an effect if the package is
    available and loaded. For details, see the \sQuote{details}
    section.}
  \item{...}{Additional arguments passed for the \pkg{multicore} package
    if used.For details, see the \sQuote{details} section.}
}

\section{Methods}{
  Identify TSS:
  \describe{
    \item{identify:}{
      \code{signature(obj="TssNorm")}
    }{
      \code{identify(obj, threshold=1, exppara=c(20, 20),
	fun=subtractExpectation, multicore=TRUE, ...)}
    }
  }
}

\details{
  The \code{identify} method identifies TSS in the normalized read data.

  In order to speed up the identification step, the \code{mclapply}
  function of \pkg{multicore} can be used. For this, the \pkg{multicore}
  package has to be loaded manully before starting the computation,
  additional parameters are passed via the \code{...} argument, e.g. as
  \code{normalize(obj, mc.ncores=2)}. The \code{multicore} argument can
  further be used to temporarily disable the parallel estimation by
  setting it to \code{FALSE}. Pleas note that the identification step in
  normally very fast and thus using parallelized computation here may a
  minor impact as compared to the \code{normalize} method.
}

\value{
  An object of class \code{TssResult}.
}

\author{
  Maintainer: Julian Gehring <julian.gehring@fdm.uni-freiburg.de>
}

\examples{
example(normalize)

## identify TSS
z <- identify(yFit)
z
}

\keyword{methods}
\keyword{models}

