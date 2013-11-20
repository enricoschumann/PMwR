\name{btest}
\alias{btest}
\title{
  Backtest
}
\description{
  Testing trading strategies.
}
\usage{
btest(prices, signal,
      do.signal = TRUE, do.rebalance = TRUE, print.info = NULL,
      b = 1L, fraction = 1,
      initial.position = 0, initial.cash = 0, tc = 0, ...,
      add = FALSE,
      adjust.signal = NULL,
      tradeOnOpen = TRUE,
      tol = 1e-5, assignInGlobals = list(),
      prices0 = NULL,include.data = FALSE)
}
\arguments{
  \item{prices}{
    
    For a single asset, a matrix of prices with four columns: open,
    high, low and close. For \code{n} assets, a list of length four:
    \code{prices[[1]]} is then a matrix with \code{n} columns containing
    the open prices for the assets; \code{prices[[2L]]} is a matrix with
    the high prices, and so on. If only close prices are used, then for
    a single asset, either a matrix of one column or a numeric vector;
    for multiple assets a list of length one, containing the matrix of
    close prices. (For example, with 100 close prices of 5 assets, the
    prices should be arranged in a matrix \code{p} of size 100 times 5;
    and \code{prices = list(p)}.)

  }
  \item{signal}{

    A function that evaluates to the position suggested by the trading
    rule; see Details.

  }
  \item{do.signal}{

    logical or a function that evaluates to \code{TRUE} or \code{FALSE};
    default is \code{TRUE}.

  }
  \item{do.rebalance}{    

    logical or a function that evaluates to \code{TRUE} or
    \code{FALSE}. If \code{TRUE} (the default), the portfolio is
    rebalanced in any period.

  }
  \item{print.info}{
    A function. If \code{NULL}, nothing is printed. See Details.
  }
  \item{b}{
    burnin (an integer). Defaults to 1.
  }
  \item{fraction}{
    amount of rebalancing to be done (a scalar between 0 and 1)
  }
  \item{initial.position}{
    initial portfolio (a numeric vector).  If supplied, this will also
    be the initial suggested position.
  }
  \item{initial.cash}{
    a numeric vector of length 1. Defaults to 0.
  }
  \item{tc}{
    transaction costs in percent
  }
  \item{\dots}{
    other named arguments. All functions (signal, signalYN, rebalanceYN,
    printInfo) will have access to these arguments.
  }
  \item{add}{

    Default is \code{FALSE}. \code{TRUE} is \strong{not implemented}
    (but would mean that \code{signal} should evaluate to changes in
    position, ie, orders).

  }
  \item{adjust.signal}{

    Default is \code{NULL}. If \code{fixedPosition}, any positive signal
    will be transformed into \code{positionSize}.  If \code{weight} and
    \code{positionSize} is \code{NULL}, any output of signal will be
    considered weights. If positionSize is not \code{NULL}, signals will
    be transformed in fixed weights (\code{positionSize}). See Details.
    
  }
  \item{tradeOnOpen}{
    A logical vector of length one; default is \code{TRUE}.
  }
  \item{tol}{
    
    A numeric vector of length one: only rebalance if the maximum
    absolute suggested change in any position is greater than
    \code{tol}. Default is 0.00001 (which practically means always
    rebalance).

  }
  \item{assignInGlobals}{

    A \code{list} of named elements. See Details.

  }
  \item{prices0}{

    A numeric vector (default is \code{NULL}). Only used if \code{b} is 0 and
    an initial portfolio \code{x0} is specified.

  }
  \item{include.data}{

    logical
    
  }
}
\details{
  
  The function provides an accounting framework for trading rules. The
  basic ingredients are time series that are supposed to consist of
  \acronym{OHLC} bars (these bars need actually not be equally spaced)
  and several functions that map these series into positions.

  \code{backtest} runs a loop from \code{b + 1L} to
  \code{NROW(prices)}. In a given iteration \code{t} it should should
  use information from periods prior to \code{t} (but nothing will
  ensure this); trading then takes place at the opening price of
  \code{t}. For slow-to-compute signals this is reasonable if there is
  time lag between close and open (eg, for daily prices). For higher
  frequencies such as every second, the signal function should be fast to
  compute; alternatively, it may be better to use a larger time offset
  (ie, use information from periods \code{<< t}).

  If no \acronym{ohlc} bar are available, a single series (assumed to be
  close prices) can be passed.  These will then be expanded. In effect, we
  might then use information up to the close of \code{t-1L}, and trade at
  the close of \code{t}.

  Precomputed Signals: given a vector of signals -1, 0, and 1, it will
  typically not be efficient to compute those within the loop (though it
  is possible and often clearer; in particular, it will mimic the logic
  needed in a realtime system). Then the signal vector can be passed as an
  argument, and \code{signalYN} should be a function that returns
  \code{FALSE}.

  Reserved names are currently these: "Open", "High", "Low", "Close",
  "Wealth", "Cash", "Time", "Portfolio", "SuggestedPortfolio",
  "Globals".

}
\value{
  A list.
  \item{\code{portfolio}}{The actual portfolio holdings}
  \item{\code{suggested.portfolio}}{The suggested holdings}
  \item{\code{cash}}{cash}
  \item{\code{wealth}}{total portfolio value}

  and  cumTC, trades, initial.wealth
}
\references{
  Enrico Schumann. Portfolio Managemnt with R.
}
\author{
  Enrico Schumann
}
\note{
  This function is still very experimental and will likely change.
}


\examples{
## example 1: MACD on a single asset


## example 2: Gamma trading


## example 3: portfolio optimisation


}
