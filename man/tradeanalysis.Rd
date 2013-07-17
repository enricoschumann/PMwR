\name{TradeAnalysis}
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
    a vector. 
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
scaleTrades(n,p,timestamp, TRUE)  ## each _trade_ gets scaled
}
