\name{TssNorm-class}

\docType{class}

\alias{TssNorm}
\alias{TssNorm-class}
\alias{normalize}
\alias{normalize,TssData-method}
\alias{ratio}
\alias{ratio,TssNorm-method}
\alias{fit}
\alias{fit,TssNorm-method}
\alias{parameters}
\alias{parameters,TssNorm-method}
\alias{plot,TssNorm-method}
\alias{show,TssNorm-method}

\title{
  Class \code{TssNorm}
}

\description{
  TssNorm
}

\section{Slots}{
  \describe{
    \item{\code{reads}:}{reads}
    \item{\code{segments}:}{segments}
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

    plot method:
    \item{plot:}{
      \code{signature(obj="TssNorm"}
    }
    
    get methods:
    \item{ratio:}{
      \code{signature(obj="TssNorm"}
    }
    \item{fit:}{
      \code{signature(obj="TssNorm"}
    }
  }
}

\examples{
example(TssData)

y4 <- normalize(x4)
y4

y5 <- normalize(x5, fit=TRUE)
y5

plot(y5, 1)
}

\keyword{classes}
\keyword{methods}
