\name{btest}
\alias{btest}
\concept{Backtesting}
\title{
  Backtesting Investment Strategies
}
\description{
  Testing trading and investment strategies.
}
\usage{
btest(prices, signal, do.signal = TRUE, do.rebalance = TRUE,
      print.info = NULL, b = 1L, fraction = 1,
      initial.position = 0, initial.cash = 0,
      final.position = FALSE,
      cashflow = NULL, tc = 0, \dots,
      add = FALSE, lag = 1, convert.weights = FALSE, tradeOnOpen = TRUE,
      tol = 1e-5, assignInGlobals = list(),
      prices0 = NULL, include.data = FALSE, timestamp, instrument,
      progressBar = FALSE)
}
\arguments{
  \item{prices}{
    
    For a single asset, a matrix of prices with four columns: open,
    high, low and close. For \code{n} assets, a list of length four:
    \code{prices[[1]]} is then a matrix with \code{n} columns containing
    the open prices for the assets; \code{prices[[2]]} is a matrix with
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

    Logical/numeric vector or function that evaluates to \code{TRUE} or
    \code{FALSE}. Default is \code{TRUE}: a signal is then computed in
    every period. When a logical/numeric vector, its length must match
    the number of observations in prices: \code{so.signal} then gives
    the rows in \code{prices} at which a signal is computed.  Can also
    be the string \dQuote{firstofmonth} or \dQuote{lastofmonth}; in this
    case, \code{timestamp} needs to specified and must be coercable to
    \code{\link{Date}}.

  }
  \item{do.rebalance}{    
    Same as \code{do.signal}. Can also be the string "do.signal", in
    which case the actual result from \code{do.signal} is copied.
  }
  \item{print.info}{
    A function. If \code{NULL}, nothing is printed. See Details.
  }
  \item{cashflow}{
    A function. Default is \code{NULL}.
  }
  \item{b}{
    burn-in (an integer). Defaults to 1.
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
  \item{final.position}{
    logical
  }
  \item{tc}{
    transaction costs as a fraction of turnover (e.g., 0.001 means 0.1\%)
  }
  \item{\dots}{
    other named arguments. All functions (signal, signalYN, rebalanceYN,
    printInfo) will have access to these arguments. See Details for
    reserved argument names.
  }
  \item{add}{

    Default is \code{FALSE}. \code{TRUE} is \strong{not implemented} --
    but would mean that \code{signal} should evaluate to \emph{changes}
    in position, i.e., orders.

  }
  \item{lag}{default is 1}
  \item{convert.weights}{

    Default is \code{FALSE}.  If \code{TRUE}, the value of signal will
    be considered a weight vector and automatically translated into
    (fractional) position sizes.    

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

    A numeric vector (default is \code{NULL}). Only used if \code{b} is
    0 and an initial portfolio (\code{initial.position}) is specified.

  }
  \item{include.data}{

    logical. If \code{TRUE}, all passed data are stored in final
    \code{btest} object. (See Value.)
    
  }
  \item{timestamp}{

    a vector of timestamps, along prices (optional; mainly used for
    print method and journal)
    
  }
  \item{instrument}{

    character vector of instrument names (optional; mainly used for
    print method and journal)
    
  }
  \item{progressBar}{      
    logical: display \code{\link{txtProgressBar}}?
  }

}
\details{
  
  The function provides an accounting framework for trading rules. The
  basic ingredients are time series in the form of \acronym{OHLC} bars
  (which need not be equally spaced) and several functions that map
  these series into positions.
  
  \code{backtest} runs a loop from \code{b + 1} to
  \code{NROW(prices)}. In iteration \code{t}, a \code{signal} can be
  computed based on information from periods prior to \code{t} (but
  nothing will ensure this). Trading then takes place at the opening
  price of \code{t}. For slow-to-compute signals this is reasonable if
  there is time lag between close and open. For daily prices, for
  instance, signals could be computed overnight. For higher frequencies,
  such as every second, the signal function should be fast to compute;
  alternatively, it may be better to use a larger time offset (i.e., use
  information from periods \code{<< t}).

  If no \acronym{ohlc} bar are available, a single series (assumed to be
  close prices) can be passed. In effect, we might then use information
  up to the close of \code{t - 1}, and trade at the close of \code{t}.
  
  Reserved names are currently these: "Open", "High", "Low", "Close",
  "Wealth", "Cash", "Time", "Timestamp", "Portfolio",
  "SuggestedPortfolio", "Globals".

}
\value{
  A list with class attribute \code{btest}. The list comprises:  
  \item{\code{portfolio}}{The actual portfolio holdings}
  \item{\code{suggested.portfolio}}{The suggested holdings}
  \item{\code{cash}}{cash}
  \item{\code{wealth}}{total portfolio value}

  and cumTC, trades, initial.wealth. When \code{include.data} is
  \code{TRUE}, all information (prices, timestamp, etc.) are stored in
  the list as well.

}
\references{
Schumann, E. (2016) \emph{Portfolio Management
  with R}. \url{http://enricoschumann.net/R/packages/PMwR/manual/PMwR.html}

} 
\author{
Enrico Schumann <es@enricoschumann.net>
}
\examples{
## For more examples, please see then Manual
## http://enricoschumann.net/R/packages/PMwR/manual/PMwR.html

timestamp <- structure(c(16679L, 16680L, 16681L, 16682L, 
                         16685L, 16686L, 16687L, 16688L, 
                         16689L, 16692L, 16693L), 
                       class = "Date")
prices <- c(3182, 3205, 3272, 3185, 3201, 
            3236, 3272, 3224, 3194, 3188, 3213)
data.frame(timestamp, prices)


signal <- function()     ## buy when last price is 
    if (Close() < 3200)  ## below 3200, else sell
        1 else 0         ## (more precisely: build position of 1
                         ##  when price < 3200, else reduce
                         ##  position to 0)

solution <- btest(prices = prices, signal = signal)
journal(solution)


## with Date timestamps
solution <- btest(prices = prices, signal = signal,
                  timestamp = timestamp)
journal(solution)
}
