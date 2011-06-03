\name{TssData-class}

\docType{class}

\alias{TssData}
\alias{TssData-class}
\alias{show}
\alias{show,TssData-method}

\title{
  Class \code{TssData}
}

\description{
  Class \code{TssData} storing raw sequence read data.
}

\section{Slots}{
  \describe{
    \item{\code{reads}:}{List with the read data for each sagment.}
    \item{\code{segments}:}{Data frame with the information of the
      individual segments.}
    \item{\code{annotation}:}{Annotation data object, as passed through the
      \code{annotation} argument.}
    \item{\code{timestamp}:}{Timestamp with the creation time of the object.}
  }
}

\section{Methods}{

  segmentize methods:
  \describe{
    \item{segmentize:}{
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
      \code{signature(obj="TssData")}
    }{
      Get the read start sites. The second argument selects the
      individual segment; if missing returns a list containing the
      information of all segments.
    }
    
    \item{end:}{
      \code{signature(obj="TssData")}
    }{
      Get the read end sites, see \code{start}.
    }
    
    \item{counts:}{
      \code{signature(obj="TssData")}
    }{
      Get the raw read counts, see \code{start}.
    }
    
    \item{reads:}{
      \code{signature(obj="TssData")}
    }{
      Get all the read data of the segments, including
      e.g. \sQuote{start}, \sQuote{counts}, \sQuote{replicate}, , see
      \code{start}.
    }
    
    \item{segments:}{
      \code{signature(obj="TssData")}
    }{
      Get the information associated with the segments, e.g. chromosome,
      strand, region. The second and third argment select the segment
      and the variable of interest.
    }
    
    \item{timestamp:}{
      \code{signature(obj="TssData")}
    }{
      Get the creation timestamp of the object.
    }
    
    \item{annotation:}{
      \code{signature(obj="TssData")}
    }{
      Get the annotation data, as passed through the \code{annotation}
      argument.
    }

    \item{[:}{
      \code{signature(obj="TssData")}
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
showClass("TssData")
}

\keyword{classes}
\keyword{methods}
