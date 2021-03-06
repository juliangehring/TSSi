\name{normalizeCounts-methods}

\docType{methods}

\alias{normalizeCounts}
\alias{normalizeCounts-TSSi}
\alias{normalizeCounts,TssData-method}
\alias{normalizeCounts-methods}

\alias{assessPoisson}
\alias{assessGradPoisson}

\title{
  Normalize methods
}

\description{
  Normalize sequence read count data.
}

\usage{
normalizeCounts(x, fun=mean, offset=10L, basal=1e-4, lambda=c(0.1, 0.1),
fit=FALSE, multicore=TRUE, optimizer="all", ...)
}

\arguments{
  \item{x}{Object of class \code{TssData} with raw data to normalize.}
  \item{fun}{Function used to average over replicates (default:
    \code{mean}).}
  \item{offset}{Integer defining the number of bases add to the ends of
    each segment with \code{basal} rate.}
  \item{basal}{Numeric specifying the basal rate.}
  \item{lambda}{Numeric vector of length two specifying the regulation
    parameter for each side of the segment.}
  \item{fit}{Logical whether the fitting should be performed in addition
    to the estimation based on the Poisson ratios obtained from all
    reads.}
  \item{multicore}{Logical whether to use the \pkg{parallel} package to
    speed up the fitting. Has only an effect if the package is
    available and loaded. For details, see the \sQuote{details}
    section.}
  \item{optimizer}{Character string choosing the optimizer for the
    fit (default: \dQuote{all}). Possible choices are \dQuote{optim} for
    the \code{optim} function from the \pkg{stats} package,
    \dQuote{bobyqa} for the \code{bobyqa} function from the
    \pkg{minqu} package,or \dQuote{all} for taking the best fit out of
    both.}
  \item{...}{Additional arguments passed for the \pkg{parallel} package
    if used. For details, see the \sQuote{details} section.}
}

\section{Methods}{
  Normalize read data:
  \describe{
    \item{normalizeCounts:}{
      \code{signature(x="TssData")}
    }{
      \code{normalizeCounts(x, ...)}
    }
  }
}

\details{
  The normalization reduces the noise by shrinking the counts towards
  zero. This step is intended to eliminate false positive counts as well
  as making further analyzes more robust by reducing the impact of large
  counts. Such a shrinkage or regularization procedure constitutes a
  well-established strategy in statistics to make predictions
  conservative, i.e. to reduce the number of false positive predictions.
  
  An objective function is minimized to estimate the transcription level
  in a regularized manner. The log-likelihood is given by the product of
  the probabilities of the counts which is assumed as a Poisson
  distribution by default.

  For \eqn{\lambda_1 > 0}{\sQuote{lambda[1] > 0}}, counts unequal to
  zero are penalized to obtain conservative estimates of the
  transcription levels with a preferably small number components,
  i.e. genomic positions, unequal to zero. The larger
  \eqn{\lambda_1}{\sQuote{lambda[1]}}, the more conservative is the
  identification procedure.

  To enhance the shrinkage of isolated counts in comparison to counts in
  regions of strong transcriptional activity, the information of
  consecutive genomic positions in the measurements is regarded by
  evaluating differences between adjacent count estimates.

  In order to distribute the identification step over multiple processor
  cores, the \code{mclapply} function of the \pkg{parallel} package can
  be used. For this, the \pkg{parallel} package has to be loaded
  manually before starting the computation, additional parameters are
  passed via the \code{...} argument, e.g.as \code{normalizeCounts(x,
    mc.cores=2)}. The \code{multicore} argument can further be used to
  temporarily disable the parallel estimation by setting it to
  \code{FALSE}.
}

\value{
  An object of class \code{TssNorm}.
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
example(segmentizeCounts)

## normalize data, w/o and w/ fitting
yRatio <- normalizeCounts(x)
yFit <- normalizeCounts(x, fit=TRUE)

yFit

\dontrun{
## parallel computation
library(parallel)
yFit <- normalizeCounts(x, fit=TRUE, mc.ncores=2)
}
}

\keyword{methods}
\keyword{models}

