\name{backtest}
\alias{backtest}
\title{
  Accounting for Trades
}
\description{
  Accounting for trades.
}
\usage{
backtest(prices, signal,
         signalYN = TRUE, rebalanceYN = TRUE, printInfo = NULL,
         b = 1L, phi = 1, x0 = 0, c0 = 100, tc = 0, ...,
         addToPosition = FALSE,
         adjustSignal = NULL, positionSize = NULL,
         tradeOnOpen = TRUE,
         tol = 1e-5, assignInGlobals = list(),
         prices0 = NULL)
}
\arguments{
  \item{prices}{
    For a single asset, a matrix of prices with four columns: open,
    high, low and close. For \code{n} assets, a list of length four:
    prices[[1]] is then a matrix with \code{n} columns containing the
    open prices for the assets; prices[[2L]] is a matrix with the high
    prices, and so on. If only close prices are used: for a single
    asset, either a matrix of one column or a numeric vector; for
    multiple assets a list whose single element is a matrix. (For
    example, with 100 close prices of 5 assets, the prices should be
    arranged in a matrix \code{p} of size 100 times 5; and \code{prices
      = list(p)}.
  }
  \item{signal}{
    a function
  }
  \item{signalYN}{
    a function
  }
  \item{rebalanceYN}{
    a function. If TRUE (the default), the portfolio is rebalanced in any period.
  }
  \item{printInfo}{
    a function. If missing, nothing is printed.
  }
  \item{b}{
    burnin (a scalar). Defaults to 0.
  }
  \item{phi}{
    amount of rebalancing to be done (a scalar between 0 and 1)
  }
  \item{x0}{
    initial portfolio
  }
  \item{c0}{
    initial cash. Defaults to 100; but this is probably not a reasonable value.
  }
  \item{tc}{
    transaction costs in percent
  }
  \item{\dots}{
    other named arguments. All functions (signal, signalYN, rebalanceYN,
    printInfo) will have access to these arguments.
  }
  \item{addToPosition}{
    default is FALSE. Not implemented.
  }
  \item{adjustSignal}{

    Default is \code{NULL}. If \code{fixedPosition}, any positive signal
    will be transformed into \code{positionSize}.  If \code{weight} and
    \code{positionSize} is \code{NULL}, any output of signal will be
    considered weights. If positionSize is not \code{NULL}, signals will
    be transformed in fixed weights (\code{positionSize}).
  }
  \item{positionSize}{

    default is \code{NULL}. Only used if \code{adjustSignal} is either
    \code{fixedPosition} or \code{weight}.

  }
  \item{tradeOnOpen}{a logical vector of length one.}
  \item{tol}{
    a numeric vector of length one. Only rebalance if the maximum
    absolute suggested change in any position is greater than
    \code{tol}. Default is 0.00001 (which basically means always
    rebalance).
  }
  \item{assignInGlobals}{a list}
  \item{prices0}{a numeric vector (default is \code{NULL})}
}
\details{
  The function provides an \sQuote{accounting} framework for
  trading rules. One basic ingredients are a time series that is supposed to
  consists of \acronym{OHLC} bars. These bars need actually not be equally
  spaced, but for now we assume they are.

  The function runs a loop from \code{b + 1L} to \code{nrow(prices)}. In a
  given iteration \code{t} it should should use information from periods
  \code{< t} (but nothing will ensure this); trading then takes place at
  the open of \code{t}. For slow-to-compute signals this is reasonable if
  there is time lag between close and open (eg, for daily prices). For
  higher frequencies (eg, every second), the signal function should be
  fast to compute; alternatively, it may be better to use a larger time
  offset (ie, use information from periods \code{<< t}).

  If no \acronym{ohlc} bar are available, a single series (assumed to be
  close prices) can be passed.  These will then be expanded. In effect, we
  might then use information up to the close of \code{t-1L}, and trade at
  the close of \code{t}.

  Precomputed Signals: given a vector of signals -1, 0, and 1, it will
  typically not be efficient to compute those within the loop (though it
  is possible and often clearer; in particular, it will mimic the logic
  needed in a realtime system). Then the signal vector can be passed as an
  argument, and \code{signalYN} should be a function that returns
  \code{FALSE}.  For those

  Reserved names are currently these: "Open", "High", "Low", "Close",
  "Wealth", "Cash", "Time", "Portfolio", "SuggestedPortfolio",
  "Globals".

}
\value{
  A list.
  \item{\code{X}}{The actual portfolio holdings}
  \item{\code{Xs}}{The suggested holdings}
}
\references{
  Enrico Schumann. Portfolio Managemnt with R.
}
\author{
  Enrico Schumann
}
\note{
  Very experimental.
}


\examples{
## example 1: MACD on a single asset


## example 2: Gamma trading


## example 3: portfolio optimisation


}
