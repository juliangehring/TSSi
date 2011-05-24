\name{get-methods}

\docType{methods}

\alias{get-methods}
\alias{start}
\alias{start,TssData-method}
\alias{end}
\alias{end,TssData-method}
\alias{counts}
\alias{counts,TssData-method}
\alias{reads}
\alias{reads,TssData-method}
\alias{segments}
\alias{segments,TssData-method}
\alias{timestamp}
\alias{timestamp,TssData-method}
\alias{annotation}
\alias{annotation,TssData-method}
\alias{names}
\alias{names,TssData-method}
\alias{ratio}
\alias{ratio,TssNorm-method}
\alias{fit}
\alias{fit,TssNorm-method}
\alias{parameters}
\alias{parameters,TssNorm-method}
\alias{delta}
\alias{delta,TssResult-method}
\alias{expect}
\alias{expect,TssResult-method}
\alias{tss}
\alias{tss,TssResult-method}

\title{
  Get methods
}

\description{
  Get methods for objects of the classes \code{TssData}, \code{TssNorm},
  and \code{TssResult}.
}

\section{Methods}{
  
  class \code{TssData}, \code{TssNorm}, \code{TssResult}:
  \describe{
    \item{start:}{
      \code{signature(obj="TssData")}
    }{
      Get the read start sites. The second argument selects the
      individual segment; if missing returns a list containing the
      information of all segments.
    }
    
    \item{end:}{
      \code{signature(obj="TssData")}
    }{
      Get the read end sites, see \code{start}.
    }

    \item{counts:}{
      \code{signature(obj="TssData")}
    }{
      Get the raw read counts, see \code{start}.
    }
    
    \item{reads:}{
      \code{signature(obj="TssData")}
    }{
      Get all the read data of the segments, including
      e.g. \sQuote{start}, \sQuote{counts}, \sQuote{replicate}, , see
      \code{start}.
    }
    
    \item{segments:}{
      \code{signature(obj="TssData")}
    }{
      Get the information associated with the segments, e.g. chromosome,
      strand, region. The second and third argment select the segment
      and the variable of interest.
    }
    
    \item{timestamp:}{
      \code{signature(obj="TssData")}
    }{
      Get the creation timestamp of the object.
    }
    
    \item{annotation:}{
      \code{signature(obj="TssData")}
    }{
      Get the annotation data, as passed through the \code{annotation}
      argument.
    }
  }
  
  class \code{TssNorm}, \code{TssResult}:
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
  }
  
  class \code{TssResult}:
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
example(segmentize)

## some examples for get methods
start(x)
head(start(x, 1))

head(reads(x, 1))

segments(x)
names(x)
}

\keyword{methods}
