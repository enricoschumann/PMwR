\name{unit_prices}
\alias{unit_prices}
\title{
  Compute Prices for Portfolio Based on Units
}
\description{
  Compute prices for a portfolio based on outstanding shares.
}
\usage{
unit_prices(NAV,
            cashflows,
            initial.price, initial.shares = 0,
            cf.included = TRUE)
}
\arguments{
  \item{NAV}{
    a dataframe of two columns: timestamp and net asset value
}
  \item{cashflows}{
    a data.frame of two or three columns: timestamp,
    cashflow and (optionally) an id
}
  \item{initial.price}{
    initial price
}
  \item{initial.shares}{
    number of outstanding shares for first NAV
}
  \item{cf.included}{
    logical
}
}
\details{

  \strong{This function is experimental, and its interface
  is not stable.}

  The function may be used to compute the returns for a
  portfolio with external cashflows, i.e. what is
  usually called time-weighted returns.

  Valuation (i.e. the computation of the NAV) must take
  place before external cashflows. Fairness suggests
  that: what price would you give an external investor
  if you had not valued the positions? And even if
  fairness mattered not: suppose we traded on a
  specific day, had a positive PL, and ended the day in
  cash. We could then not differentiate any more
  between a cash increase because of an external inflow
  and a cash increase because of a profitable trade.

}
\value{
  A data.frame
  \item{timestamp}{}
  \item{NAV}{total NAV}
  \item{price}{NAV per share}
  \item{units}{outstanding units (i.e. shares) after cashflows}

  Attached as an attribute is a \code{\link{data.frame}}
  \code{transactions}.

}
\references{

  Schumann, E. (2020) \emph{Portfolio Management with
    \R}. \url{http://enricoschumann.net/PMwR/}

}
\author{
  Enrico Schumann
}
\seealso{
  \code{\link{returns}}, \code{\link{pl}}
}
\examples{
NAV <- data.frame(timestamp = seq(as.Date("2017-1-1"),
                                  as.Date("2017-1-10"),
                                  by = "1 day"),
                  NAV = c(100,101:104,205:209))

cf <- data.frame(timestamp = c(as.Date("2017-1-1"),
                               as.Date("2017-1-6")),
                 cashflow = c(100, 100))

unit_prices(NAV, cf, cf.included = TRUE)
}
