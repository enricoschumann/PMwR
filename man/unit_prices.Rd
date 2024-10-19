\name{unit_prices}
\alias{unit_prices}
\title{
  Compute Prices for Portfolio Based on Units
}
\description{
  Compute prices for a portfolio based on outstanding shares
  (units).
}
\usage{
unit_prices(NAV, cashflows,
            initial.price, initial.units = 0,
            cf.included = TRUE,
            round.price = NULL, round.units = NULL)
}
\arguments{
  \item{NAV}{%
    a dataframe of two columns: timestamp and net asset
    value. There should be no duplicated timestamps.  Column
    names are ignored; the function assumes timestamp is the
    first column, \acronym{NAV} the second.
  }
  \item{cashflows}{%
    a data.frame of two or three columns: timestamp,
    cashflow and (optionally) an id or account. Column names
    are ignored; the function assumes timestamp is the first
    column, the external cashflows the second, and an
    account/id the third.
  }
  \item{initial.price}{%
    initial price; ignored when \code{initial.units} is not
    zero
  }
  \item{initial.units}{%
    number of outstanding units before first cashflow
  }
  \item{cf.included}{%
    \code{\link{logical}}. If \code{\link{TRUE}} (the
    default), it is assumed that the NAV series at the time
    of the cashflow already includes the cashflow.
  }
  \item{round.price}{%
    round unit prices: \code{NULL} (no rounding) or an
    integer
  }
  \item{round.units}{%
    round number of units: \code{NULL} (no rounding) or an
    integer
  }
}
\details{%

  \strong{This function is experimental, and its interface
  is not stable yet.}

  The function may be used to compute the returns for a
  portfolio with external cashflows, i.e. what is usually
  called time-weighted returns.  Note that 'cashflows' can
  also comprise other positions that are added or removed
  from the portfolio without affecting performance.

}
\value{%
  A \code{\link{data.frame}} with one row for each row in
  \code{NAV}:

  \item{timestamp}{the timestamp, as provided in argument
    \code{NAV}}

  \item{NAV}{total NAV, as provided in argument \code{NAV}}

  \item{price}{NAV per unit}

  \item{units}{outstanding units (i.e. shares) \emph{after}
    cashflows}

  Attached as an attribute is a \code{\link{data.frame}}
  \code{transactions}, with as many rows as the provided
  argument \code{cashflows}, which provides the number of
  units created/destroyed for each cashflow.

}
\references{

  Schumann, E. (2024) \emph{Portfolio Management with \R}.\cr
  \url{https://enricoschumann.net/PMwR/}

}
\author{
  Enrico Schumann
}
\seealso{
  \code{\link{returns}}, \code{\link{pl}}
}
\examples{
NAV <- data.frame(timestamp = seq(as.Date("2017-01-01"),
                                  as.Date("2017-01-10"),
                                  by = "1 day"),
                  NAV = c(100:104, 205:209))

cf <- data.frame(timestamp = c(as.Date("2017-01-01"),
                               as.Date("2017-01-06"),
                               as.Date("2017-01-06")),
                 cashflow = c(100, 50, 50),
                 account = c("A", "A", "B"))

(up <- unit_prices(NAV, cf, cf.included = TRUE))
##     timestamp NAV    price    units
## 1  2017-01-01 100 100.0000 1.000000
## 2  2017-01-02 101 101.0000 1.000000
## 3  2017-01-03 102 102.0000 1.000000
## 4  2017-01-04 103 103.0000 1.000000
## 5  2017-01-05 104 104.0000 1.000000
## 6  2017-01-06 205 105.0000 1.952381
## 7  2017-01-07 206 105.5122 1.952381
## 8  2017-01-08 207 106.0244 1.952381
## 9  2017-01-09 208 106.5366 1.952381
## 10 2017-01-10 209 107.0488 1.952381

attr(up, "transactions")
##    timestamp cashflow account     units
## 1 2017-01-01      100       A 1.0000000
## 2 2017-01-06       50       A 0.4761905
## 3 2017-01-06       50       B 0.4761905
}
