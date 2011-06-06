\name{asRangedData-methods}

\docType{methods}

\alias{asRangedData}
\alias{asRangedData-methods}
\alias{readsAsRangedData}
\alias{readsAsRangedData,TssData-method}
\alias{segmentsAsRangedData}
\alias{segmentsAsRangedData,TssData-method}
\alias{tssAsRangedData}
\alias{tssAsRangedData,TssResult-method}

\title{
  Convert to \code{RangedData}
}

\description{
  Convert data and results to objects of class \code{RangedData}.
}

\section{Methods}{

  \describe{
    \item{readsAsRangedData:}{
      \code{signature(obj="TssData")}
    }{
      Convert the reads, as obtained by the \code{reads} method, to an
      object of class \code{RangedData}.
    }

    \item{segmentsAsRangedData:}{
      \code{signature(obj="TssData")}
    }{
      Convert the segments, as obtained by the \code{segments} method,
      to an object of class \code{RangedData}.
    }
    
    \item{tssAsRangedData:}{
      \code{signature(obj="TssResult")}
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
example(segmentize)

readsRd <- readsAsRangedData(x)
head(readsRd)

segmentsRd <- segmentsAsRangedData(x)
head(segmentsRd)
}

\keyword{methods}
