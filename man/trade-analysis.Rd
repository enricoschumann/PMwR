\name{Trade-Analysis}
\alias{close_on_first}
\alias{limit}
\alias{scale_to_unity}
\alias{scale_trades}
\alias{split_trades}
\alias{tw_exposure}
\title{
  Analysing Trades: Compute Profit/Loss, Resize and more
}
\description{
  Functions to help analyse trades (as opposed to profit-and-loss series)
}
\usage{
scale_trades(amount, price, timestamp, aggregate = FALSE,
            fun = NULL, ...)
split_trades(amount, price, timestamp, aggregate = FALSE,
             drop.zero = FALSE)

limit(amount, price, timestamp, lim, tol = 1e-8)
scale_to_unity(amount)
close_on_first(amount)

tw_exposure(amount, timestamp, start, end, abs.value = TRUE)
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
  \item{drop.zero}{
    logical. If \code{TRUE}, trades with zero amounts are removed.
    See Examples.
  }
  \item{abs.value}{
    logical. If \code{TRUE}, the absolute exposure is computed.
  }
  \item{\dots}{passed on to fun}
  \item{tol}{numeric}
}
\details{

\code{scale_trades} takes a vector of notionals, prices and
scales all trades along the paths so that the maximum
exposure is 1.

The default \code{fun} divides every element of a vector
\code{n} by \code{max(abs(cumsum(n)))}. If user-specified,
the function \code{fun} needs to take a vector of notionals
(changes in position.)

\code{split_trades} decomposes a trade list into single
trades, where a single trade comprises those trades from a
zero position to the next zero position.  Note that the
trades must be sorted chronologically.

}
\value{

  Either a list or a list-of-lists.

}
\author{
  Enrico Schumann
}
\seealso{
  \code{\link{btest}}
}
\examples{
n <- c(1,1,-3,-1,2)
p <- 100 + 1:length(n)
timestamp <- 1:length(n)

scale_trades(n, p, timestamp)
scale_trades(n, p, timestamp, TRUE)  ## each _trade_ gets scaled

split_trades(n, p, timestamp)
split_trades(n, p, timestamp, TRUE)  ## almost like the original series



## effect of 'drop.zero'
P <- c(100, 99, 104, 103, 102, 105, 104)  ## price series
S <- c(  0,  1,   1,   0,   0,   1,   0)  ## position to be held
dS <- c(0, diff(S)) ## change in position ==> trades
t <- seq_along(P)

#### ==> 1) with all zero amounts
split_trades(amount = dS, price = P, timestamp = t)

#### ==> 2) without zero-amount trades
split_trades(amount = dS, price = P, timestamp = t, drop.zero = TRUE)

#### ==> 3) without all zero-amounts
zero <- dS == 0
split_trades(amount = dS[!zero], price = P[!zero], timestamp = t[!zero])

}
