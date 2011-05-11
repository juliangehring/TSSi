\name{TssNorm-class}

\docType{class}

\alias{TssNorm}
\alias{TssNorm-class}
\alias{normalize}
\alias{normalize,TssData-method}

\title{
  Class \code{TssNorm}
}

\description{
  TssNorm
}

\section{Slots}{
  \describe{
    \item{\code{reads}:}{reads}
    \item{\code{regions}:}{regions}
    \item{\code{annotation}:}{annotation}
    \item{\code{timestamp}:}{timestamp}
    \item{\code{parameters}:}{parameters}
  }
}

\section{Methods}{
  \describe{
    \item{normalize:}{
      \code{obj="TssData"}
    }
  }
}

\examples{
example(TssData)

y4 <- normalize(x4)
y4

y5 <- normalize(x5, fit=TRUE)
y5
}

\keyword{classes}
\keyword{methods}
