\name{physcoCounts-data}

\docType{data}

\alias{physcoCounts}
\alias{physcoCounts-TSSi}
\alias{physcoCounts-data}

\title{CAP capture data}

\description{
  Data of a 5'-CAP capture experiment, after mapping the reads to the
  genome.
}

\usage{data(physcoCounts)}

\format{
  \describe{
    \item{physcoCounts}{Data frame with results from a 5'-CAP capture
      experiment, with the columns:}
    \describe{
      \item{chromosome}{Chromosome the read is mapped to.}
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

\details{
  The transcription data from Physcomitrella patens was mapped using the
  bowtie software. Then, the positions of 5' ends of each read were
  extracted and the number of reads at each position counted.

  For further details, please see the publication.
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
## load data set
data(physcoCounts)
}

\source{
  Rensing et al., 2011.

  in preparation
}

\keyword{datasets}

