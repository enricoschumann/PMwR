\name{returns}
\alias{returns}
\alias{.returns}
\alias{p_returns}
\alias{returns.default}
\alias{returns.zoo}
\alias{print.p_returns}
\alias{toLatex.p_returns}
\alias{toHTML.p_returns}
\title{
  Compute Returns
}
\description{
  Convert prices into returns.
}
\usage{
returns(x, \dots)

\method{returns}{default}(x, t = NULL, period = NULL, complete.first = TRUE,
                          pad = NULL,
                          position = NULL, weights = NULL, rebalance.when = NULL,
                          lag = 1, \dots)

\method{returns}{zoo}(x, period = NULL, complete.first = TRUE,
                      pad = NULL, position = NULL, lag = 1, \dots)

\method{print}{p_returns}(x, \dots, year.rows = TRUE, month.names = NULL,
                         zero.print = "0", plus = FALSE, digits = 1)

\method{toLatex}{p_returns}(object, \dots,
       year.rows = TRUE, ytd = "YTD", month.names = NULL, 
       eol = "\\\\\\\\", stand.alone = FALSE)

\method{toHTML}{p_returns}(x, \dots,
    year.rows = TRUE, ytd = "YTD", month.names = NULL, 
    stand.alone = TRUE, table.style = NULL, table.class = NULL, 
    th.style = NULL, th.class = NULL,
    td.style = "text-align:right;padding:0.5em;", 
    td.class = NULL, tr.style = NULL, tr.class = NULL)

.returns(x, pad = NULL, lag)
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
    
       typically character, currently either "month", "year" or "ann"
       (for annualised). Returns will not be annualised when the period
       is less than one year. To force annualising, use "ann!".
       used when timestamp information is available (eg, \code{t} is not
       \code{NULL} or with \code{zoo} objects).

     }
  \item{complete.first}{
    logical. For monthly returns, should the first month (if incomplete)
    be used.
  }
  \item{pad}{
    either \code{NULL} (no padding of initial lost observation) or a
    value used for padding (reasonable values might be \link{NA} or 0)
  }
  \item{position}{
    numeric; the same length/dimension as
    \code{x}. \strong{Deprecated: will be removed in the future.}
  }
  \item{weights}{
    numeric
  }
  \item{rebalance.when}{
    numeric
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
    an object of class \code{p_returns} (\sQuote{period returns})
  }
  \item{month.names}{
    character: names of months
  }
  \item{digits}{
    number of digits in table
  }
  \item{ytd}{
    header for YTD
  }
  \item{eol}{character}
  \item{stand.alone}{logical or character}
  \item{table.class}{character}
  \item{table.style}{character}
  \item{th.class}{character}
  \item{th.style}{character}
  \item{td.class}{character}
  \item{td.style}{character}
  \item{tr.class}{character}
  \item{tr.style}{character}
}
\details{

  \code{return} is a generic function.  It computes simple returns:
  current values divided by prior values minus one.  The default method
  works for numeric vectors/matrices.

}
\value{

  If called as \code{returns(x)}: a numeric vector or matrix, possibly
  of class \code{zoo}.

  If called with a \code{period} argument: an object of class
  \code{"p_returns"} (period returns), which is a numeric vector of
  returns with attributes \code{t} (timestamp) and \code{period}.
  Main use is to have methods that pretty-print such period returns;
  currently, there are methods
  for \code{\link{toLatex}} and \code{\link[tools]{toHTML}}.

}
\author{
  Enrico Schumann
}
\seealso{
  \code{\link{btest}}
}
\examples{
x <- 101:105
returns(x)
returns(x, pad = NA)
returns(x, pad = NA, lag = 2)

t <- seq(as.Date("2012-01-05"), as.Date("2012-12-31"), by = "1 day")
x <- seq_along(t) + 1000
returns(x, t = t, period = "month")
returns(x, t = t, period = "month", complete.first = FALSE)
}
