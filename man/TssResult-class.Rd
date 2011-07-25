\name{TssResult-class}

\docType{class}

\alias{TssResult}
\alias{TssResult-class}
\alias{show,TssResult-method}

\title{
  Class \code{TssResult}
}

\description{
  Class \code{TssResult} storing final sequence read data with
  identified TSS.
}

\section{Slots}{
  \describe{
    \item{\code{reads}:}{List with the read data for each segment.}
    \item{\code{segments}:}{Data frame with the information of the
      individual segments.}
    \item{\code{tss}:}{List of the identified TSS locations.}
    \item{\code{annotation}:}{Annotation data object, as passed through the
      \code{annotation} argument.}
    \item{\code{timestamp}:}{Timestamp with the creation time of the object.}
    \item{\code{parameters}:}{List with the parameters.}
  }
}

\section{Methods}{
  All methods for class \code{TssNorm}, as well as:
  
  identify methods:
  \describe{
    \item{identifyStartSites:}{
      \code{signature(x="TssNorm")}
    }{
      Identify TSS in the normalized data.
    }
  }

  get methods:
  \describe{
    \item{expect:}{
      \code{signature(x="TssResult")}
    }{
      Get the expectation for non-specific reads, see\code{start}.
    }
    
    \item{tss:}{
      \code{signature(x="TssResult")}
    }{
      Get the identified transcription start sites, see\code{start}.
    }

    \item{[:}{
      \code{signature(x="TssResult")}
    }{
      Subset the object, by name or index.
    }
  }

  asRangedData methods:
  \describe{
    \item{tssAsRangedData:}{
      \code{signature(x="TssResult")}
    }{
      Convert the tss predictions, as obtained by the \code{tss} method,
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
  \code{\link[TSSi]{plot}}, \code{\link[TSSi]{asRangedData-methods}}

  Functions:
  \code{\link[TSSi]{subtract-functions}}

  Data:
  \code{\link[TSSi]{readData}}

  Package:
  \code{\link[TSSi]{TSSi-package}}
}

\examples{
showClass("TssResult")
}

\keyword{classes}
\keyword{methods}
