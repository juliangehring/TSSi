\name{readData-data}

\docType{data}

\alias{readData}
\alias{readData-data}

\title{CAP capture data}

\description{
  Results from a 5'-CAP capture experiment, after mapping the reads to
  the genome.
}

\usage{data(readData)}

\format{
  \describe{
    \item{readData}{Data frame with results from a 5'-CAP capture
      experiment, with the columns:}
    \describe{
      \item{chromosome}{Chromosomes}
      \item{region}{Predefined region based on annotations, which can be treated
	independently in the analysis.}
      \item{start}{Start position, given as bp, of the 5' end of the
	read.}
      \item{strand}{Forward and reverse strand, given as '+' and '-',
	respectively.}
      \item{counts}{Number of reads at the respective position.}
    }
  }
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
## load data
data(readData)
}

\source{Rensing et al., 2010}

\keyword{datasets}

