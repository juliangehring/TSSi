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
  *missing*

  The package is published under the GPL-3 license.
}

\author{
  Clemens Kreutz, Julian Gehring, Jens Timmer

  Maintainer: Julian Gehring <julian.gehring@fdm.uni-freiburg.de>
}

\references{
  to be published
}

\seealso{
  Package:
  \code{\link[TSSi]{TSSi-package}}

  Methods:
  \code{\link[TSSi]{segmentize}}, \code{\link[TSSi]{normalize}},
  \code{\link[TSSi]{identify}}, \code{\link[TSSi]{get-methods}},
  \code{\link[TSSi]{plot}}, \code{\link[TSSi]{asRangedData-methods}}

  Functions:
  \code{\link[TSSi]{subtract-functions}}

  Classes:
  \code{\linkS4class{TssData}}, \code{\linkS4class{TssNorm}},
  \code{\linkS4class{TssResult}} 

  Data set:
  \code{\link[TSSi]{readData}}
}

\examples{
## load data set
data(readData)

## segmentize data
attach(readData)
x <- segmentize(counts=counts, start=start, chr=chromosome,
region=region, strand=strand)
detach(readData)

x
segments(x)

## normalize data, w/o and w/ fitting
yRatio <- normalize(x)
yFit <- normalize(x, fit=TRUE)
yFit

## identify TSS
z <- identify(yFit)
z

## inspect results
head(tss(z, 1))
plot(z, 1)
}

\keyword{package}
\keyword{htest}
\keyword{models}