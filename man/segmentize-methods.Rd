\name{segmentize-methods}

\docType{methods}

\alias{segmentize}
\alias{segmentize,integer,integer-method}
\alias{segmentize-methods}

\title{
  Segmentize methods
}

\description{
  Import sequence read count data and transform it into segments.
}

\usage{
segmentize(counts, start, end=start, chr=rep(1L, length(start)),
region=rep(1L, length(start)), strand=rep("*", length(start)),
replicate=rep(1L, length(start)), annotation=NULL, ...)
}

\arguments{
  \item{counts}{Integer vector with the number of reads for each
    position in \code{start}.}
  \item{start}{Integer vector with the start positions of the reads.}
  \item{end}{Optional integer vector with the end positions of the
    reads. If not supplied the values of \code{start} will be used.}  
  \item{chr}{Optional vector with the chromosomal locations of the
    reads. If not supplied all reads will are assumed to be located on
    one chromosome.}
  \item{region}{Optional vector with an assignment of each read to a
    seperate region, for example based on additional annotation. If not
    supplied all reads will be part of one region.}
  \item{strand}{Optional vector with the strand location of each
    read.}
  \item{replicate}{Optional integer vector identifying the replicate each
    read was obtained from, in the case of data from multiple
    measurements.}
  \item{annotation}{Optional object containing metadata passed along it
    the analysis. This argument does not influence the analysis.}
  \item{...}{Optional arguments.}
  \describe{
    \item{pattern}{Regular expression specifying the naming of the
      segments. The terms \sQuote{\%1$s}, \sQuote{\%2$s}, and
      \sQuote{\%3$s} refer to the chromosome, the strand, and the
      region, respectively. If not supplied the standard naming pattern
      \sQuote{\%1$s_\%2$s_\%3$s} will be used.}
  }
}

\section{Methods}{
  Import read data and transform it into segments:
  \describe{
    \item{segmentize:}{
      \code{signature(nReads="integer", start="integer")}
    }
    {
      \code{segmentize(counts, start, ...)}
    }
  }
}

\details{
  The \code{segmentize} method takes the raw data and breaks it into
  segments which will be analyzed seperately in the subsequent
  steps. Segments are defined in a way such that any has a unique
  combination of the input arguments \code{chr}, \code{region}, and
  \code{strand}. In case any of these is not supplied it is assumend
  that all reads belong to one chromosome, region, or strand,
  respectively. Usage of the \code{region} argument is beneficial if the
  location of potential TSS can be contrained below the level of
  chromosomes and strands.
}

\value{
  An object of class \code{TssData}.
}

\author{
  Maintainer: Julian Gehring <julian.gehring@fdm.uni-freiburg.de>
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
example(readData)

## import and segmentize data
attach(readData)
x <- segmentize(counts=counts, start=start, chr=chromosome,
region=region, strand=strand)
detach(readData)

x
}

\keyword{methods}
\keyword{IO}

