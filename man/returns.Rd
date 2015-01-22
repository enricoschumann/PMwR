\name{returns}
\alias{returns}
\alias{preturns}
\alias{returns.default}
\alias{returns.zoo}
\alias{print.preturns}
\alias{toLatex.preturns}
\alias{toHTML.preturns}
\title{
  Compute Returns
}
\description{
  Convert prices into returns.
}
\usage{
returns(x, \dots)

\method{returns}{default}(x, \dots, t = NULL, period = NULL, complete.first = TRUE,
                          pad = NULL, position, lag = 1)

\method{returns}{zoo}(x, \dots, period = NULL, complete.first = TRUE,
                      pad = NULL, position, lag = 1)

\method{print}{preturns}(x, \dots, year.rows = TRUE, month.names = NULL,
                         zero.print = "0", plus = FALSE, digits = 1)

\method{toLatex}{preturns}(object, \dots,
       year.rows = TRUE, ytd = "YTD", month.names = NULL, 
       eol = "\\\\\\\\")

\method{toHTML}{preturns}(x, \dots,
    year.rows = TRUE, ytd = "YTD", month.names = NULL, 
    stand.alone = TRUE, table.style = NULL, table.class = NULL, 
    th.style = NULL, th.class = NULL,
    td.style = "text-align:right;padding:0.5em;", 
    td.class = NULL, tr.style = NULL, tr.class = NULL)

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
    \code{x}. \strong{Deprecated: will be removed in the future.}
  }
  \item{\dots}{
    further arguments
  }
  \item{year.rows}{
    logical. If \code{TRUE} (the default), print monthly returns with
    one row per year.
  }  
  \item{zero.print}{
    character.  How to print zero values.
  }  
  \item{plus}{
    logical. Add a \sQuote{\code{+}} before positive numbers? Default is
    \code{FALSE}.
  }  
  \item{lag}{
    The lag for computing returns. A positive integer, defaults to one;
    ignored for time-weighted returns or if \code{t} is supplied.
  }  
  \item{object}{
    an object of class \code{preturns} (\sQuote{period returns})
  }
}
\details{

  return is a generic function.  It computes simple returns: current values divided by
  prior values minus one.  The default method works for numeric
  vectors/matrices.
  
}
\value{

  If called as \code{returns(x)}: a numeric vector or matrix.

  If called with a \code{period} argument: an object of class
  \code{"preturns"} (period returns), which is a list of the dates, the
  returns and the period.  Main use is to have methods that pretty-print
  such period returns; currently, there are methods for
  \code{\link{toLatex}} and \code{\link[tools]{toHTML}}.
  
}
\author{
  Enrico Schumann
}
\seealso{
  \code{\link{btest}}
}
\examples{
x <- 101:112
returns(x)
returns(x, pad = NA)
returns(x, pad = NA, lag = 2)

t <- seq(as.Date("2012-01-05"), as.Date("2012-12-31"), by = "1 day")
x <- seq_along(t) + 1000
returns(x, t = t, period = "month")
returns(x, t = t, period = "month", complete.first = FALSE)
}
