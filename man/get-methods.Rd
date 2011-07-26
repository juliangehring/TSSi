\name{get-methods}

\docType{methods}

\alias{get-TSSi}
\alias{get-methods}
\alias{start-TSSi}
\alias{start,TssData-method}
\alias{end-TSSi}
\alias{end,TssData-method}
\alias{counts}
\alias{counts-TSSi}
\alias{counts,TssData-method}
\alias{reads}
\alias{reads-TSSi}
\alias{reads,TssData-method}
\alias{segments}
\alias{segments-TSSi}
\alias{segments,TssData-method}
\alias{timestamp}
\alias{timestamp-TSSi}
\alias{timestamp,TssData-method}
\alias{annotation}
\alias{annotation-TSSi}
\alias{annotation,TssData-method}
\alias{names-TSSi}
\alias{names,TssData-method}
\alias{ratio}
\alias{ratio-TSSi}
\alias{ratio,TssNorm-method}
\alias{fit}
\alias{fit-TSSi}
\alias{fit,TssNorm-method}
\alias{parameters}
\alias{parameters-TSSi}
\alias{parameters,TssData-method}
\alias{delta}
\alias{delta-TSSi}
\alias{delta,TssResult-method}
\alias{expect}
\alias{expect-TSSi}
\alias{expect,TssResult-method}
\alias{tss}
\alias{tss-TSSi}
\alias{tss,TssResult-method}
\alias{[,TssData-method}
\alias{[,TssNorm-method}
\alias{[,TssResult-method}

\title{
  Get methods
}

\description{
  Get methods for objects of the classes \code{TssData}, \code{TssNorm},
  and \code{TssResult}.
}

\section{Methods}{
  
  For class \code{TssData}, \code{TssNorm} (inherited), \code{TssResult}
  (inherited):
  \describe{
    \item{start:}{
      \code{signature(x="TssData")}
    }{
      Get the read start sites. The second argument selects the
      individual segment; if missing returns a list containing the
      information of all segments.
    }
    
    \item{end:}{
      \code{signature(x="TssData")}
    }{
      Get the read end sites; see \code{start}.
    }

    \item{counts:}{
      \code{signature(x="TssData")}
    }{
      Get the raw read counts; see \code{start}.
    }
    
    \item{reads:}{
      \code{signature(x="TssData")}
    }{
      Get all the read data of the segments, including
      e.g. \sQuote{start}, \sQuote{counts}, \sQuote{replicate}; see
      \code{start}.
    }
    
    \item{segments:}{
      \code{signature(x="TssData")}
    }{
      Get the information associated with the segments, e.g. chromosome,
      strand, region. The optional second and third arguments select the
      segment and the variable of interest.
    }
    
    \item{timestamp:}{
      \code{signature(x="TssData")}
    }{
      Get the creation timestamp of the object.
    }
    
    \item{annotation:}{
      \code{signature(x="TssData")}
    }{
      Get the annotation data, as passed through the \code{annotation}
      argument.
    }
    
    \item{[:}{
      \code{signature(x="TssData")}
    }{
      Subset the object, by name or index.
    }
  }
  
  For class \code{TssNorm}, \code{TssResult} (inherited):
  \describe{
    \item{ratio:}{
      \code{signature(x="TssNorm")}
    }{
      Get the normalized reads based on the Poisson ratios;
      see \code{start}.
    }
    
    \item{fit:}{
      \code{signature(x="TssNorm")}
    }{
      Get the normalized reads based on the fit; see \code{start}.
    }
  }
  
  For class \code{TssResult}:
  \describe{
    \item{expect:}{
      \code{signature(x="TssResult")}
    }{
      Get the expectation for non-specific reads; see \code{start}.
    }
    
    \item{tss:}{
      \code{signature(x="TssResult")}
    }{
      Get the identified transcription start sites; see \code{start}.
    }
  }
}

\value{
  A data frame or list
}

\author{
  Maintainer: Julian Gehring <julian.gehring@fdm.uni-freiburg.de>
}

\seealso{
  Classes:
  \code{\linkS4class{TssData}}, \code{\linkS4class{TssNorm}},
  \code{\linkS4class{TssResult}}
  
  Methods:
  \code{\link[TSSi]{segmentizeCounts}}, \code{\link[TSSi]{normalizeCounts}},
  \code{\link[TSSi]{identifyStartSites}}, \code{\link[TSSi]{get-methods}},
  \code{\link[TSSi]{plot-methods}}, \code{\link[TSSi]{asRangedData-methods}}

  Functions:
  \code{\link[TSSi]{subtract-functions}}

  Data:
  \code{\link[TSSi]{readData}}

  Package:
  \code{\link[TSSi]{TSSi-package}}
}

\examples{
example(segmentizeCounts)

## some examples for get methods
start(x)
head(start(x, 1))

head(reads(x, 1))

segments(x)
names(x)
}

\keyword{methods}
