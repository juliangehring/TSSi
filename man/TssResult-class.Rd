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
  }
}

\author{
  Maintainer: Julian Gehring <julian.gehring@fdm.uni-freiburg.de>
}

\examples{
showClass("TssResult")
}

\keyword{classes}
\keyword{methods}
