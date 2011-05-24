\name{mTSS-package}

\docType{package}

\alias{mTSS}
\alias{mTSS-package}

\title{Transcription Start Sites Identification}

\description{
  The \pkg{mTSS} package normalizes and detects transcription start
  sites in sequencing data.
}

\details{
  The \pkg{mTSS} package normalizes and detects transcription start
  sites in sequencing data.

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
}

\examples{
## load data
data(readData)
readData <- readData[1:500, ]

## import and segmentize data
attach(readData)
x <- segmentize(counts=reads, start=start, chr=chromosome,
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