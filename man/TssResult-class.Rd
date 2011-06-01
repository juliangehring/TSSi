\name{TssResult-class}

\docType{class}

\alias{TssResult}
\alias{TssResult-class}
\alias{show,TssResult-method}

\title{
  Class \code{TssResult}
}

\description{
  Class \code{TssResulting} storing final sequence read data with
  identified TSS.
}

\section{Slots}{
  \describe{
    \item{\code{reads}:}{List with the read data for each sagment.}
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
    \item{identify:}{
      \code{signature(obj="TssNorm")}
    }{
      Identify TSS in the normalized data.
    }
  }

  get methods:
  \describe{
    \item{expect:}{
      \code{signature(obj="TssResult")}
    }{
      Get the expectation for non-specific reads, see\code{start}.
    }
    
    \item{tss:}{
      \code{signature(obj="TssResult")}
    }{
      Get the identified transcription start sites, see\code{start}.
    }

    \item{[:}{
      \code{signature(obj="TssResult")}
    }{
      Subset the object, by name or index.
    }
  }
}

\author{
  Maintainer: Julian Gehring <julian.gehring@fdm.uni-freiburg.de>
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

  Package:
  \code{\link[mTSS]{mTSS-package}}
}

\examples{
showClass("TssResult")
}

\keyword{classes}
\keyword{methods}
