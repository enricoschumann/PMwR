\name{TradeAnalysis}
\alias{PLsorted}
\alias{pl}
\alias{closeOnFirst}
\alias{limit}
\alias{scaleToUnity}
\alias{scaleTrades}
\alias{splitTrades}
\alias{twExposure}
\title{
  Analysing Trades: Compute Profit/Loss, Resize and more
}
\description{
  Functions to help analyse trades (as opposed to profit-and-loss series)
}
\usage{
pl(amount, price,
   instrument = NULL, timestamp = NULL,
   along.timestamp = FALSE, do.sort = FALSE, initcash = 0,
   tol = 1e-10,
   aggr.accounts = FALSE, account.sep = "::")

PLsorted(amount, price, timestamp = NULL,
         allprices = NULL, alltimes = NULL,
         initcash = 0, do.sort = FALSE)

scaleTrades(amount, price, timestamp, aggregate = FALSE,
            fun = NULL, ...)
splitTrades(amount, price, timestamp, aggregate = FALSE)

limit(amount, price, timestamp, lim, tol = 1e-8)
scaleToUnity(amount)
closeOnFirst(amount)

twExposure(amount, timestamp, start, end, abs.value = TRUE)
}
\arguments{
  \item{amount}{
    notionals
  }
  \item{price}{
    a vector of prices
  }
  \item{timestamp}{
    a vector. If \code{timestamp} is not specified for \code{PLsorted},
    no mapping to a time series will take place.
  }
  \item{aggregate}{
    \code{TRUE} or \code{FALSE}
  }
  \item{fun}{
    a function
  }
  \item{lim}{
    a maximum absolute position size
  }
  \item{start}{
    optional time
  }
  \item{end}{
    optional time
  }
  \item{abs.value}{
    logical. If \code{TRUE}, the absolute exposure is computed.
  }
  \item{instrument}{
    a character vector
  }
  \item{allprices}{
    a vector
  }
  \item{alltimes}{
    a vector
  }
  \item{initcash}{
    a numeric vector of length one
  }
  \item{do.sort}{
    sort all by time
  }
  \item{x}{
    a \code{numeric} vector (possibly with a \code{dim} attribute; ie, a
    matrix)
  }
  \item{pad}{
    fill initial lost observation for returns. Default is \code{NULL}.
  }
  \item{\dots}{passed on to fun}
  \item{tol}{numeric}
}
\details{

\code{scaleTrades} takes a vector of notionals, prices and scales all
trades along the paths so that the maximum exposure is 1.

The default \code{fun} divides every element of a vector \code{n} by
\code{max(abs(cumsum(n)))}. If user-specified, the function \code{fun}
needs to take a vector of notionals (changes in position.)

\code{splitTrades} decomposes a trade list into single trades, where a
single trade comprises those trades from a zero position to the next
zero position.

}
\value{

  \code{pl}: an object of class \code{pl}, which is a list of five
  elements (and the class attribute).

  Either a list or a list-of-lists.

}
\author{
  Enrico Schumann
}
\seealso{
  \code{\link{backtest}}
}
\examples{
n <- c(1,1,-3,-1,2)
p <- 100 + 1:length(n)
timestamp <- 1:length(n)
splitTrades(n,p,timestamp)
splitTrades(n,p,timestamp, TRUE)  ## almost like the original series

scaleTrades(n,p,timestamp)
scaleTrades(n,p,timestamp, TRUE) ## each _trade_ gets scaled


J <- Journal(timestamp = c(1, 2, 3),
                amount = c(1, 1, -2),
                price  = c(100,102, 101))
pl(J)
pl(c(1, 1, -2), c(100,102, 101)) ## without a 'Journal'


J <- Journal(timestamp  = c(1,2,3,1,2,3),
             amount     = c(1, 1, -2, 1,1,-2),
             price      = c(100, 102, 101, 100, 102, 105),
             instrument = c(rep("A", 3), rep("B", 3)))
pl(J)



}
