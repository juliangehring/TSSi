\name{subtract-functions}

\alias{subtract-functions}
\alias{subtractExpectation}

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
  \item{bg}{Numeric vector with backgroud of the same length as
    \code{fg}.}
  \item{indTss}{Index vector indicating the expected TSS sites in
    \code{fg}.}
  \item{pos}{Positions of the reads in the segment, as given in the
    \code{segmentize} method.}
  \item{basal}{See the \code{normalize} method.}
  \item{tau}{See the \code{identify} method.}
  \item{extend}{Logical indicating whether the background estimates
    should be computed for all positions, rather than only for those
    with reads.}
}

\details{
  The \code{subtractExpectation} function is one approach on how to
  subtract the expectation value.

  Other functions with the same call structure can be used in the
  detection of the TSS by passing it as \code{fun} argument in the
  \code{identify} method.
}

\seealso{
  Classes:
  \code{\linkS4class{TssData}}, \code{\linkS4class{TssNorm}},
  \code{\linkS4class{TssResult}}
  
  Methods:
  \code{\link[TSSi]{segmentize}}, \code{\link[TSSi]{normalize}},
  \code{\link[TSSi]{identify}}, \code{\link[TSSi]{get-methods}},
  \code{\link[TSSi]{plot}}, \code{\link[TSSi]{asRangedData-methods}}

  Functions:
  \code{\link[TSSi]{subtract-functions}}

  Data:
  \code{\link[TSSi]{readData}}

  Package:
  \code{\link[TSSi]{TSSi-package}}
}

\examples{
args(subtractExpectation)
}

