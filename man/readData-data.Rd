\name{readData}

\docType{data}

\alias{readData}
\alias{readData-data}

\title{Spike-in ChIP-chip data set}

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
      \item{reads}{Number of reads at the respective position.}
    }
  }
}

\examples{
## load data
data(readData)
readData <- readData[1:500, ]
}

\source{Rensing et al., 2010}

\keyword{datasets}

