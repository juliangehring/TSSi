\name{TSSi-package}

\docType{package}

\alias{TSSi}
\alias{TSSi-package}

\title{Transcription Start Site Identification}

\description{
  The \pkg{TSSi} package normalizes and identifies transcription start
  sites in high-throughput sequencing data.
}

\details{
  High throughput sequencing has become an essential experimental
  approach for the investigation of transcriptional mechanisms. For some
  applications like ChIP-seq, there are several available approaches for
  the prediction of peak locations. However, these methods are not
  designed for the identification of transcription start sites (TSS)
  because such data sets have qualitatively different noise.

  The \pkg{TSSi} provides a heuristic framework for the identification
  of TSS based on high-throughput sequencing data. Probabilistic
  assumptions for the count distribution as well as for systematic
  errors, i.e. for contaminating measurements close to a TSS, are made
  and can be adapted by the user. The framework also comprises a
  regularization procedure which can be applied as a preprocessing step
  to decrease the noise and thereby reduce the number of false
  predictions.

  The package is published under the GPL-3 license.
}

\author{
  Clemens Kreutz, Julian Gehring, Jens Timmer

  Maintainer: Julian Gehring <julian.gehring@fdm.uni-freiburg.de>
}

\references{
  C. Kreutz, J. Gehring, D. Lang, J. Timmer, and S. Rensing:
  TSSi - An R package for transcription start site identification from
  high throughput sequencing data.
  
  in preparation
}

\seealso{
  Package:
  \code{\link[TSSi]{TSSi-package}}

  Methods:
  \code{\link[TSSi]{segmentizeCounts}}, \code{\link[TSSi]{normalizeCounts}},
  \code{\link[TSSi]{identifyStartSites}}, \code{\link[TSSi]{get-methods}},
  \code{\link[TSSi]{plot-methods}}, \code{\link[TSSi]{asRangedData-methods}}

  Functions:
  \code{\link[TSSi]{subtract-functions}}

  Classes:
  \code{\linkS4class{TssData}}, \code{\linkS4class{TssNorm}},
  \code{\linkS4class{TssResult}} 

  Data set:
  \code{\link[TSSi]{physcoCounts}}
}

\examples{
## load data set
data(physcoCounts)

## segmentize data
attach(physcoCounts)
x <- segmentizeCounts(counts=counts, start=start, chr=chromosome,
region=region, strand=strand)
detach(physcoCounts)

x
segments(x)

## normalize data, w/o and w/ fitting
yRatio <- normalizeCounts(x)
yFit <- normalizeCounts(x, fit=TRUE)
yFit

## identify TSS
z <- identifyStartSites(yFit)
z

## inspect results
head(tss(z, 1))
plot(z, 1)
}

\keyword{package}
\keyword{htest}
\keyword{models}