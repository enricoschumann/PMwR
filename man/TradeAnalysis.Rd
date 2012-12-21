\name{TradeAnalysis}
\alias{PLsorted}
\alias{PL}
\alias{closeOnFirst}
\alias{limit}
\alias{returns}
\alias{scaleToUnity}
\alias{scaleTrades}
\alias{splitTrades}
\title{
  Analysing Trades
}
\description{
  Functions to help analyse trades (as opposed to profit-and-loss series)
}
\usage{
PL(notional, prices, symbols = NULL, tol = 1e-10)
PLsorted(notional, prices, tradetimes = NULL,
         allprices = NULL, alltimes = NULL,
         initcash = 0, do.sort = FALSE)

returns(x, pad = NULL)

scaleTrades(notional, prices, tradetimes, aggregate = FALSE, fun = NULL,
...)
splitTrades(notional, prices, tradetimes, aggregate = FALSE)

limit(trade, lim, tol = 0.00000001)
scaleToUnity(n)
closeOnFirst(n)
}
\arguments{
  \item{notional}{
    notionals
  }
  \item{prices}{
    prices
  }
  \item{aggregate}{
    TRUE or FALSE
  }
  \item{fun}{
    a function
  }
  \item{symbols}{
    a character vector
  }
  \item{tradetimes}{
    a vector. If tradetimes is not specified, no mapping to a time
    series will take place.
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
  \item{x}{a \code{numeric} vector (possibly with a \code{dim} attribute; ie, a matrix)
  }
  \item{pad}{fill initial lost observation for returns. Default is \code{NULL}.
  }
  \item{\dots}{passed on to fun}
}
\details{
\code{scaleTrades} takes a vector of notionals, prices and scales all trades
along the paths so that the maximum exposure is 1.

The default \code{fun} divides every element of a vector \code{n} by
\code{max(abs(cumsum(n)))}.

The function \code{fun} needs to take a vector of notionals (changes in
position.)

}
\value{
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
tradetimes <- 1:length(n)
splitTrades(n,p,tradetimes)
splitTrades(n,p,tradetimes, TRUE)  ## almost like the original series

scaleTrades(n,p,tradetimes)
scaleTrades(n,p,tradetimes, TRUE) ## each _trade_ gets scaled
}
