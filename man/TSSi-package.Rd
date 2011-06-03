\name{TSSi-package}

\docType{package}

\alias{TSSi}
\alias{TSSi-package}

\title{Transcription Start Sites Identification}

\description{
  The \pkg{TSSi} package normalizes and detects transcription start
  sites in sequencing data.
}

\details{
  The \pkg{TSSi} package normalizes and detects transcription start
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
  \code{\linkS4class{TssData}}, \code{\linkS4class{TssNorm}},
  \code{\linkS4class{TssResult}}
  
  Methods:
  \code{\link[TSSi]{segmentize}}, \code{\link[TSSi]{normalize}},
  \code{\link[TSSi]{identify}}, \code{\link[TSSi]{get-methods}},
  \code{\link[TSSi]{plot}}

  Functions:
  \code{\link[TSSi]{subtract-functions}}

  Data:
  \code{\link[TSSi]{readData}}

  Package:
  \code{\link[TSSi]{TSSi-package}}

}

\examples{
## load data
data(readData)

## import and segmentize data
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