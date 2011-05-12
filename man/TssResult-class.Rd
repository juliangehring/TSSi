\name{TssResult-class}

\docType{class}

\alias{TssResult}
\alias{TssResult-class}
\alias{identify}
\alias{identify,TssNorm-method}
\alias{delta}
\alias{delta,TssResult-method}
\alias{expect}
\alias{expect,TssResult-method}
\alias{tss}
\alias{tss,TssResult-method}
\alias{plot,TssResult-method}

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

    plot method:
    \item{plot:}{
      \code{signature(obj="TssResult"}
    }
    
    get methods:
    \item{delta:}{
      \code{signature(obj="TssResult"}
    }
    \item{expect:}{
      \code{signature(obj="TssResult"}
    }
    \item{tss:}{
      \code{signature(obj="TssResult"}
    }
  }
}

\examples{
example(normalize)
z4 <- identify(y4)
z5 <- identify(y5)

plot(z5, 1)
}

\keyword{classes}
\keyword{methods}
