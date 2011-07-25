\name{plot-methods}

\docType{methods}

\alias{plot}
\alias{plot-methods}
\alias{plot,TssData-method}
\alias{plot,TssNorm-method}
\alias{plot,TssResult-method}

\title{
  Plot methods
}

\description{
  Plot the data and results of the \pkg{TSSi} package.
}

\usage{
plot(x, y, ...)
}

\arguments{
  \item{x}{An object of class \code{TssData}, \code{TssNorm}, or
    \code{TssResult}.}
  \item{y}{A single integer or character string specifying which segment
    to plot. An integer is interpreted as the index of the segment while
    a character string is matched against the segment names.}
  \item{...}{Optional arguments used in order to customize the figure. See
    the \sQuote{details} section.}
}

\section{Methods}{
  Visualize the raw data:
  \describe{
    \item{plot:}{
      \code{signature(x="TssData")}
    }{
      \code{plot(x, y, counts=TRUE, legend=TRUE, ...)}
    }
  }

  Visualize the normalized data:
  \describe{
    \item{plot:}{
      \code{signature(x="TssNorm")}
    }{
      \code{plot(x, y, counts=TRUE, ratio=TRUE, fit=TRUE, legend=TRUE,
	...)}
    }
  }

  Visualize the normalized data along with the identified TSS:
  \describe{
    \item{plot:}{
      \code{signature(x="TssResult")}
    }{
      \code{plot(x, y, counts=TRUE, ratio=TRUE, fit=TRUE, expect=FALSE,
	tss=TRUE, threshold=TRUE, rug=TRUE, legend=TRUE, ...)}
    }
  }

}

\details{
  With the \code{plot} method, the raw, normalized, or final data can
  easily be visualized.

  The \code{plot} method uses a special system in order to customize the
  graphical elements of the figure. It allows to refer to the different
  components with the name of the additional input argument; its value
  is a list containing named graphical parameters for the underlying
  plot function. The following list describes the available names and
  their contribution.

  \describe{
    \item{\code{plot}}{Graphical parameters for the axes and the
      labeling, passed to the \code{plot} function.}

    \item{\code{counts}}{Logical indicating whether the raw counts
      should be plotted.}
    \item{\code{countsArgs}}{Graphical parameters for the
      \sQuote{counts} variable, passed to the \code{points} function.}

    \item{\code{ratio}}{Logical indicating whether the estimates based
      on the Poisson ratios should be plotted.}
    \item{\code{ratioArgs}}{Graphical parameters for the
      \sQuote{ratio} variable, passed to the \code{points} function.}

    \item{\code{fit}}{Logical indicating whether the estimates based on
      the fitting should be plotted.}
    \item{\code{fitArgs}}{Graphical parameters for the
      \sQuote{fit} variable, passed to the \code{points} function.}

    \item{\code{expect}}{Logical indicating whether the background
      estimates should be plotted.}
    \item{\code{expectArgs}}{Graphical parameters for the
      \sQuote{expect} variable, passed to the \code{points} function.}
    \item{\code{expect}}{Logical indicating whether the background
      estimates should be computed for all positions, rather than only
      for those with reads.}

    \item{\code{tss}}{Logical indicating whether the identified TSS
      should be plotted.}
    \item{\code{tssArgs}}{Graphical parameters for the
      \sQuote{tss} variable, passed to the \code{points} function.}

    \item{\code{threshold}}{Logical indicating whether the threshold parameter
      used in the identification step should be indicated.}
    \item{\code{thresholdArgs}}{Graphical parameters for the
      \sQuote{threshold} variable, passed to the \code{abline} function.}

    \item{\code{rug}}{Logical indicating whether the location of the identified TSS
      should be indicated.}
    \item{\code{rugArgs}}{Graphical parameters for the
      \sQuote{rug} variable, passed to the \code{rug} function.}

    \item{\code{baseline}}{Logical indicating whether a baseline
      indicating zero reads should be drawn.}
    \item{\code{baselineArgs}}{Graphical parameters for the
      \sQuote{baseline} variable, passed to the \code{abline} function.}   

    \item{\code{legend}}{Logical indicating whether a legend should be
      plotted.}
    \item{\code{legendArgs}}{Graphical parameters for the
      \sQuote{legend} variable, passed to the \code{legend} function.}
  }

  Thus, for (a) omitting the ratio estimates, the threshold, and the
  legend, (b) customizing the graphical parameters of the raw read
  counts, (c) customizing the axis labels and the title, the following
  code can be used:
  
  \code{plot(x, 1, ratio=FALSE, threshold=FALSE, legend=FALSE,
    countsArgs=list(type="h", col="darkgray", pch=NA),
    plotArgs=list(xlab="Genomic position", main="TSS for segment
    's1_-_155'")}
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
## preceding steps
example(identify)

## plot
plot(yFit, 1)

## plot w/ some custom settings
plot(z, 1, ratio=FALSE, threshold=FALSE, countsArgs=list(type="h",
col="darkgray", pch=NA), plotArgs=list(xlab="Genomic position",
main="TSS for segment 's1_-_155'"))
}

\keyword{methods}
\keyword{hplot}

