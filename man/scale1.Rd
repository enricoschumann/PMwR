\name{scale1}
\alias{scale1}
\alias{scale1.default}
\alias{scale1.zoo}
\title{
  Scale Time Series
}
\description{
  Scale time series so that they can better be compared.
}
\usage{
scale1(x, \dots)

\method{scale1}{default}(x, \dots, when = "first.complete", level = 1,
                         centre = FALSE, scale = FALSE, geometric = TRUE)

\method{scale1}{zoo}(x, \dots, when = "first.complete", level = 1,
                     centre = FALSE, scale = FALSE, geometric = TRUE,
                     inflate = NULL)
}
\arguments{
  \item{x}{
    a time series
  }
  \item{when}{
    
    origin: for the default method, either character or numeric
    (integer). For the \code{zoo} method, a value that matches the class
    of the \code{index} of \code{x}; for instance, with an index of
    class \code{\link{Date}}, when should inherit from
    \code{\link{Date}}.
    
  }
  \item{level}{
    numeric
  }
  \item{centre}{
    logical
  }
  \item{scale}{
    logical or numeric
  }
  \item{geometric}{
    logical: if \code{TRUE} (the default), the geometric mean is
    deducted with \code{centre} is \code{TRUE}; if \code{FALSE},
    the arithmetic mean is used
  }
  \item{inflate}{
    numeric: an annual rate at which the series is inflated (or deflated
             if negative)
  }
  \item{\dots}{
    other arguments passed to methods
}
}
\details{
  This is a generic function, with methods for numeric vectors and
  matrices, and \code{zoo} objects.
}
\value{
An object of the same type as \code{x}.
}
\references{
  Enrico Schumann -- PMwR
}
\author{
  Enrico Schumann
}
\note{
  Experimental: interface may change.
}
\seealso{
  \code{\link{scale}}
}
\examples{
scale1(cumprod(1 + c(0, rnorm(20, sd = 0.02))), level = 100)
}
