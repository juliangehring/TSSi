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
subtractExpectation(fg, bg, indTss, basal, exppara)
}

\arguments{
  \item{fg}{Numeric vector with foreground.}
  \item{bg}{Numeric vector with backgroud of the same length as
    \code{fg}.}
  \item{indTss}{Index vector indicating the expected TSS sites in
    \code{fg}.}
  \item{basal}{See the \code{normalize} method.}
  \item{exppara}{See the \code{identify} method.}
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
  \code{\linkS4class{TssData}}
  \code{\linkS4class{TssNorm}}
  \code{\linkS4class{TssResult}}
  
  Methods:
  \code{\link[mTSS]{segmentize}}
  \code{\link[mTSS]{normalize}}
  \code{\link[mTSS]{identify}}
  
  \code{\link[mTSS]{get-methods}}
  \code{\link[mTSS]{plot}}

  Functions:
  \code{\link[mTSS]{subtract-functions}}

  Data:
  \code{\link[mTSS]{readData}}

  Package:
  \code{\link[mTSS]{mTSS-package}}
}

\examples{
args(subtractExpectation)
}

