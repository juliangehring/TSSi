\name{TssData-class}

\docType{class}

\alias{TssData}
\alias{TssData-TSSi}
\alias{TssData-class}
\alias{show-TSSi}
\alias{show,TssData-method}

\title{
  Class \code{TssData}
}

\description{
  Class \code{TssData} storing raw sequence read data.
}

\section{Slots}{
  \describe{
    \item{\code{reads}:}{List with the read data for each segment.}
    \item{\code{segments}:}{Data frame with the information of the
      individual segments.}
    \item{\code{parameters}:}{List with parameters used for the analysis.}
    \item{\code{annotation}:}{Annotation data object, as passed through the
      \code{annotation} argument.}
  }
}

\section{Methods}{

  segmentizeCounts methods:
  \describe{
    \item{segmentizeCounts:}{
      \code{signature(nReads="integer", start="integer")}
    }{
      Import raw data and divide into segments.
    }
  }

  plot method:
  \describe{
    \item{plot:}{
      \code{signature(x="TssData", y="ANY")}
    }{
      Visualize the data.
    }
  }
    
  get methods:
  \describe{
    \item{start:}{
      \code{signature(x="TssData")}
    }{
      Get the read start sites. The second argument selects the
      individual segment; if missing returns a list containing the
      information of all segments.
    }
    
    \item{end:}{
      \code{signature(x="TssData")}
    }{
      Get the read end sites, see \code{start}.
    }
    
    \item{counts:}{
      \code{signature(x="TssData")}
    }{
      Get the raw read counts, see \code{start}.
    }
    
    \item{reads:}{
      \code{signature(x="TssData")}
    }{
      Get all the read data of the segments, including
      e.g. \sQuote{start}, \sQuote{counts}, \sQuote{replicate}, , see
      \code{start}.
    }
    
    \item{segments:}{
      \code{signature(x="TssData")}
    }{
      Get the information associated with the segments, e.g. chromosome,
      strand, region. The second and third argument select the segment
      and the variable of interest.
    }

     \item{parameters:}{
      \code{signature(x="TssData")}
    }{
      Get the parameters used in the analysis.
    }   
    
    \item{annotation:}{
      \code{signature(x="TssData")}
    }{
      Get the annotation data, as passed through the \code{annotation}
      argument.
    }

    \item{[:}{
      \code{signature(x="TssData")}
    }{
      Subset the object, by name or index.
    }
  }
  
  show methods:
  \describe{
    \item{show:}{
      \code{signature(object="TssData")}
    }
  }

  asRangedData methods:
  \describe{
    \item{readsAsRangedData:}{
      \code{signature(x="TssData")}
    }{
      Convert the reads, as obtained by the \code{reads} method, to an
      object of class \code{RangedData}.
    }

    \item{segmentsAsRangedData:}{
      \code{signature(x="TssData")}
    }{
      Convert the segments, as obtained by the \code{segments} method,
      to an object of class \code{RangedData}.
    }
  }
  
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
showClass("TssData")
}

\keyword{classes}
\keyword{methods}
