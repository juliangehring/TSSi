\name{TssNorm-class}

\docType{class}

\alias{TssNorm}
\alias{TssNorm-class}
\alias{show,TssNorm-method}

\title{
  Class \code{TssNorm}
}

\description{
   Class \code{TssNorm} storing normalized sequence read data.
}

\section{Slots}{
  \describe{
    \item{\code{reads}:}{List with the read data for each sagment.}
    \item{\code{segments}:}{Data frame with the information of the
      individual segments.}
    \item{\code{annotation}:}{Annotation data object, as passed through the
      \code{annotation} argument.}
    \item{\code{timestamp}:}{Timestamp with the creation time of the object.}
    \item{\code{parameters}:}{List with the parameters.}
  }
}

\section{Methods}{

  All methods for class \code{TssData}, as well as:

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
    \item{ratio:}{
      \code{signature(obj="TssNorm")}
    }{
      Get the normalized reads based on the Poisson ratios,
      see\code{start}.
    }
    
    \item{fit:}{
      \code{signature(obj="TssNorm")}
    }{
      Get the normalized reads based on the fit, see\code{start}.
    }

    \item{[:}{
      \code{signature(obj="TssNorm")}
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
  \code{\link[TSSi]{segmentize}}
  \code{\link[TSSi]{normalize}}
  \code{\link[TSSi]{identify}}
  
  \code{\link[TSSi]{get-methods}}
  \code{\link[TSSi]{plot}}

  Functions:
  \code{\link[TSSi]{subtract-functions}}

  Data:
  \code{\link[TSSi]{readData}}

  Package:
  \code{\link[TSSi]{TSSi-package}}
}

\examples{
showClass("TssNorm")
}

\keyword{classes}
\keyword{methods}
