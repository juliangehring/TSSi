\name{subtract-functions}

\alias{subtract-functions}
\alias{subtractExpectation}
\alias{subtractExpectation-TSSi}

\title{
  Subtract functions
}

\description{
  Functions subtracting the expectation value used in the identification
  of TSS.
}

\usage{
subtractExpectation(fg, bg, indTss, pos, basal, tau, extend=FALSE)
}

\arguments{
  \item{fg}{Numeric vector with foreground.}
  \item{bg}{Numeric vector with background of the same length as
    \code{fg}.}
  \item{indTss}{Index vector indicating the expected TSS sites in
    \code{fg}.}
  \item{pos}{Positions of the reads in the segment, as given in the
    \code{segmentizeCounts} method.}
  \item{basal}{See the \code{normalizeCounts} method.}
  \item{tau}{See the \code{identifyStartSites} method.}
  \item{extend}{Logical indicating whether the background estimates
    should be computed for all positions, rather than only for those
    with reads.}
}

\details{
  The \code{subtractExpectation} function is one approach on how to
  subtract the expectation value.

  Other functions with the same call structure can be used in the
  detection of the TSS by passing it as \code{fun} argument in the
  \code{identifyStartSites} method.
}

\seealso{
  Classes:
  \code{\linkS4class{TssData}}, \code{\linkS4class{TssNorm}},
  \code{\linkS4class{TssResult}}
  
  Methods:
  \code{\link[TSSi]{segmentizeCounts}}, \code{\link[TSSi]{normalizeCounts}},
  \code{\link[TSSi]{identifyStartSites}}, \code{\link[TSSi]{get-methods}},
  \code{\link[TSSi]{plot-methods}}, \code{\link[TSSi]{asRangedData-methods}}

  Functions:
  \code{\link[TSSi]{subtract-functions}}

  Data set:
  \code{\link[TSSi]{physcoCounts}}

  Package:
  \code{\link[TSSi]{TSSi-package}}
}

\examples{
args(subtractExpectation)
}

