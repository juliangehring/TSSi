\name{TssResult-class}

\docType{class}

\alias{TssResult}
\alias{TssResult-class}
\alias{identify}
\alias{identify,TssNorm-method}

\title{
  Class \code{TssResult}
}

\description{
  TssResult
}

\section{Slots}{
  \describe{
    \item{\code{reads}:}{reads}
    \item{\code{regions}:}{regions}
    \item{\code{tss}:}{tss}
    \item{\code{annotation}:}{annotation}
    \item{\code{timestamp}:}{timestamp}
    \item{\code{parameters}:}{parameters}
  }
}

\section{Methods}{
  \describe{
    \item{identify:}{
      \code{obj="TssNorm"}
    }
  }
}

\examples{
example(normalize)
z4 <- identify(y4)
z5 <- identify(y5)
}

\keyword{classes}
\keyword{methods}
