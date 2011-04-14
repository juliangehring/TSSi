\name{TssRaw-class}

\docType{class}

\alias{TssRaw}
\alias{TssRaw-class}
\alias{Tss}
\alias{Tss,integer,integer-method}

\title{
  Class \code{TssRaw}
}

\description{
  TssRaw
}

\section{Slots}{
  \describe{
    \item{\code{data}:}{data}
    \item{\code{region}:}{region}
    \item{\code{chr}:}{chr}
    \item{\code{date}:}{date}
  }
}

\section{Methods}{
  \describe{
    \item{Tss:}{
      \code{signature(nReads="integer", start="integer"}
    }
  }
}

\examples{
x <- Tss(nReads=10:1, start=1:10, region=rep(1:2, 5))
x
}

\keyword{classes}
\keyword{methods}
