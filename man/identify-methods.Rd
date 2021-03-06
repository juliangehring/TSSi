\name{idenfify-methods}

\docType{methods}

\alias{identifyStartSites}
\alias{identifyStartSites-TSSi}
\alias{identifyStartSites,TssData-method}
\alias{idenfify-methods}

\title{
  Identify methods
}

\description{
  Identify transcription start sites in sequence read count data.
}

\usage{
identifyStartSites(x, threshold=1, tau=c(20, 20), neighbor=TRUE,
fun=subtractExpectation, multicore=TRUE, ...)
}

\arguments{
  \item{x}{Object of class \code{TssNorm} with normalized data.}
  \item{threshold}{Numeric with the minimal number of reads to be
    treated as a potential TSS.}
  \item{tau}{Numeric vector of length two specifying the
    \eqn{\tau}{\sQuote{tau}} parameter of the exponential function for
    each side of the segment. For the forward strand (\dQuote{+}), the
    first and second value refer to the side towards the 5' and 3' end,
    respectively. In the case that a single value is provided it is
    applied to both sides.}
  \item{neighbor}{Logical whether the background estimates should be
    iteratively assigned to the predicted TSS during the estimation
    (default: TRUE).}
  \item{fun}{Function to calculate the expectation for each TSS. For
    details, see the \sQuote{details} section.}
  \item{multicore}{Logical whether to use the \pkg{parallel} package to
    speed up the computation. Has only an effect if the package is
    available and loaded. For details, see the \sQuote{details}
    section.}
  \item{...}{Additional arguments passed for the \pkg{parallel} package
    if used. For details, see the \sQuote{details} section.}
}

\section{Methods}{
  Identify TSS:
  \describe{
    \item{identifyStartSites:}{
      \code{signature(x="TssData")}
    }{
      \code{identifyStartSites(x, ...)}
    }
  }
}

\details{
  After normalization of the count data, an iterative algorithm is
  applied for each segment to identify the TSS.

  The expected number of false positive counts is initialized with a
  default value given by the read frequency in the whole data set. The
  position with the largest counts above is identified as a TSS, if the
  expected transcription level is at least one read above the expected
  number of false positive reads. The transcription levels for all TSS
  are calculated by adding all counts to their nearest neighbor TSS.

  Then, the expected number of false positive reads is updated by
  convolution with exponential kernels. The decay rates \eqn{\tau}{tau}
  in 3' direction and towards the 5'-end can be chosen differently to
  account for the fact that false positive counts are preferably found
  in 5' direction of a TSS. This procedure is iterated as long as the
  set of TSS increases.

  In order to distribute the identification step over multiple processor
  cores, the \code{mclapply} function of the \pkg{parallel} package can
  be used. For this, the \pkg{parallel} package has to be loaded
  manually before starting the computation, additional parameters are
  passed via the \code{...} argument, e.g.as \code{normalizeCounts(x,
    mc.cores=2)}. The \code{multicore} argument can further be used to
  temporarily disable the parallel estimation by setting it to
  \code{FALSE}. Pleas note that the identification step is normally very
  fast and thus using parallel computation here may a minor impact
  as compared to the \code{normalizeCounts} method.
}

\value{
  An object of class \code{TssResult}.
}

\author{
  Maintainer: Julian Gehring <julian.gehring@fdm.uni-freiburg.de>
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
## preceding steps
example(normalizeCounts)

## identify TSS
z <- identifyStartSites(yFit)

z
}

\keyword{methods}
\keyword{models}

