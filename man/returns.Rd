\name{returns}
\alias{returns}
\alias{print.preturns}
\title{
  Compute Returns
}
\description{
  Convert prices into returns.
}
\usage{
returns(x, t = NULL, period = NULL, complete.first = TRUE,
        pad = NULL,  position, lag = 1)

\method{print}{preturns}(x, \dots, year.rows)

}
\arguments{
  \item{x}{
    a \code{numeric} vector (possibly with a \code{dim} attribute; ie, a
    matrix) of prices.  Can also be of class \code{zoo}, in which case
    argument \code{t} should be \code{NULL}.
  }
  \item{t}{
        timestamps. See argument \code{"period"}.
  }
  \item{period}{
       typically character: predefined are "month" and "day". Only
       used if \code{t} is not \code{NULL}.
  }
  \item{complete.first}{
    logical. 
  }
  \item{pad}{    
    either \code{NULL} (no padding of initial lost observation) or a
    value used for padding (reasonable values might be \link{NA} or 0)
  }
  \item{position}{
    numeric; the same length/dimension as
    \code{x}. \strong{Experimental.}
  }
  \item{\dots}{
    further arguments
  }
  \item{year.rows}{
    logical. If \code{TRUE} (the default), print monthly returns with
    one row per year.
  }  
  \item{lag}{
    The lag for computing returns. A positive integer, defaults to one;
    ignored for time-weighted returns or if \code{t} is supplied.
  }  
}
\details{

  The function calculates simple returns: current values divided by
  prior values minus one.
  
}
\value{

  If called as \code{returns(x)}: a numeric vector or matrix.

  If called with a \code{time} argument, an object of class
  \code{"preturns"} (period returns), which is a list of the dates, the
  returns and the period.
  
}
\author{
  Enrico Schumann
}
\seealso{
  \code{\link{btest}}
}
\examples{

require("PMwR")
x <- 101:112
returns(x)
returns(x, pad = NA)
returns(x, pad = NA, lag = 2)

t <- seq(as.Date("2012-01-05"), as.Date("2012-12-31"), by = "1 day")
x <- seq_along(t) + 1000
returns(x, t = t, period = "month")
returns(x, t = t, period = "month", complete.first = FALSE)

}
