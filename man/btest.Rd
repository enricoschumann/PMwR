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
btest(prices, signal,
      do.signal = TRUE, do.rebalance = TRUE,
      print.info = NULL, b = 1, fraction = 1,
      initial.position = 0, initial.cash = 0,
      final.position = FALSE,
      cashflow = NULL, tc = 0, \dots,
      add = FALSE, lag = 1, convert.weights = FALSE,
      trade.at.open = TRUE, tol = 1e-5, Globals = list(),
      prices0 = NULL,
      include.data = FALSE, include.timestamp = TRUE,
      timestamp, instrument,
      progressBar = FALSE)
}
\arguments{
  \item{prices}{
    
    For a single asset, a matrix of prices with four columns: open,
    high, low and close. For \code{n} assets, a list of length four:
    \code{prices[[1]]} is then a matrix with \code{n} columns containing
    the open prices for the assets; \code{prices[[2]]} is a matrix with
    the high prices, and so on. If only close prices are used, then for
    a single asset either a matrix of one column or a numeric vector;
    for multiple assets a list of length one, containing the matrix of
    close prices. For example, with 100 close prices of 5 assets, the
    prices should be arranged in a matrix \code{p} of size 100 times 5;
    and \code{prices = list(p)}.

    The series in \code{prices} are used both as transaction prices
    and for valuing open positions. If signals are to
    be based on other series, such other series
    should be passed via the \dots argument.
    
  }
  \item{signal}{

    A function that evaluates to the position suggested by the trading
    rule; see Details.

  }
  \item{do.signal}{

    Logical/numeric vector or function that evaluates to \code{TRUE} or
    \code{FALSE}. Default is \code{TRUE}: a signal is then computed in
    every period. When a logical/numeric vector, its length must match
    the number of observations in prices: \code{do.signal} then gives
    the rows in \code{prices} at which a signal is computed.  Can also
    be the string \dQuote{firstofmonth}, \dQuote{lastofmonth}, 
    \dQuote{firstofquarter} or \dQuote{lastofquarter};
    in these cases, \code{timestamp} needs to
    specified and must be coercable to \code{\link{Date}}.

  }
  \item{do.rebalance}{    
    Same as \code{do.signal}. Can also be the string \code{"do.signal"},
    in which case the value of \code{do.signal} is copied.
  }
  \item{print.info}{
    A function or \code{NULL}. If \code{NULL}, nothing is printed. See Details.
  }
  \item{cashflow}{
    A function or \code{NULL} (default).
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
    
    other named arguments. All functions (signal,
    do.signal, do.rebalance, print.info, cashflow) will
    have access to these arguments. See Details for
    reserved argument names.

  }
  \item{add}{

    Default is \code{FALSE}. \code{TRUE} is \strong{not implemented} --
    but would mean that \code{signal} should evaluate to \emph{changes}
    in position, i.e. orders.

  }
  \item{lag}{default is 1}
  \item{convert.weights}{

    Default is \code{FALSE}.  If \code{TRUE}, the value of signal will
    be considered a weight vector and automatically translated into
    (fractional) position sizes.    

  }
  \item{trade.at.open}{
    A logical vector of length one; default is \code{TRUE}.
  }
  \item{tol}{
    
    A numeric vector of length one: only rebalance if the maximum
    absolute suggested change in any position is greater than
    \code{tol}. Default is 0.00001 (which practically means always
    rebalance).

  }
  \item{Globals}{

    A \code{list} of named elements. See Details.

  }
  \item{prices0}{

    A numeric vector (default is \code{NULL}). Only used if \code{b} is
    0 and an initial portfolio (\code{initial.position}) is specified.

  }
  \item{include.data}{

    logical. If \code{TRUE}, all passed data are stored
    in final \code{btest} object. See Section
    Value. See also argument \code{include.timestamp}.
    
  }
  \item{include.timestamp}{

    logical. If \code{TRUE}, \code{timestamp} is stored
    in final \code{btest} object. If \code{timestamp}
    is missing, integers 1, 2, \ldots are used. See
    Section Value.  See also argument
    \code{include.data}.
    
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
  
  The function provides infrastructure for testing trading
  rules. Essentially, \code{btest} does accounting: keep track of
  transactions and positions, value open positions, etc.

  The basic ingredients are time series
  in the form of \acronym{OHLC} bars (which need not be
  equally spaced) and several functions that map these
  series and other pieces of information into positions.
  
  \code{backtest} runs a loop from \code{b + 1} to
  \code{NROW(prices)}. In iteration \code{t}, a
  \code{signal} can be computed based on information
  from periods prior to \code{t}. Trading then takes
  place at the opening price of \code{t}. For
  slow-to-compute signals this is reasonable if there
  is a time lag between close and open. For daily prices,
  for instance, signals could be computed
  overnight. For higher frequencies, such as every
  second, the signal function should be fast to
  compute; alternatively, it may be better to use a
  larger time offset (i.e. use information from
  periods \code{<< t}).

  If no \acronym{ohlc} bars are available, a single
  series per asset (assumed to be close prices) can be passed. In
  effect, we might then use information up to the close
  of \code{t - 1}, and trade at the close of \code{t}.

  The \sQuote{trade logic} needs to be coded in the
  function \code{signal}; arguments to that function
  should be named and need to be passed with \code{...}.
  
  Reserved argument names are currently these:
  \code{Open}, \code{High}, \code{Low}, \code{Close},
  \code{Wealth}, \code{Cash}, \code{Time},
  \code{Timestamp}, \code{Portfolio},
  \code{SuggestedPortfolio}, \code{Globals}.

  \code{Globals} is special: it is an
  \code{\link{environment}}, which can be used to
  persistently store data during the run of
  \code{btest}. Use the argument \code{Globals}
  to add initial objects.
  
}
\value{
  A list with class attribute \code{btest}. The list comprises:  
  \item{\code{portfolio}}{The actual portfolio holdings}
  \item{\code{suggested.portfolio}}{The suggested holdings}
  \item{\code{cash}}{cash}
  \item{\code{wealth}}{total portfolio value}
  \item{\code{cum.tc}}{transaction costs}
  \item{\code{journal}}{\code{\link{journal}} of trades}
  \item{\code{initial.wealth}}{initial wealth}
  \item{\code{b}}{burnin}
  \item{\code{final.position}}{final position}
  \item{\code{Globals}}{environment Globals}

  When \code{include.data} is \code{TRUE}, all information (prices,
  timestamp, instrument, the actual \code{call} and functions
  \code{signal} etc.) are stored in the list as well.

}
\references{
Schumann, E. (2018) \emph{Portfolio Management
  with R}. \url{http://enricoschumann.net/PMwR/}
} 
\author{
  Enrico Schumann <es@enricoschumann.net>
}
\examples{
## For more examples, please see then Manual
## http://enricoschumann.net/R/packages/PMwR/manual/PMwR.html

## 1 - a simple rule
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



## 2 - a simple MA model
\dontrun{
library("PMwR")
library("NMOF")

dax <- DAX[[1]]

n <- 5
ma <- MA(dax, n, pad = NA)

ma_strat <-  function(ma) {
    if (Close() > ma[Time()])
        1
    else
        0
}


plot(as.Date(row.names(DAX)), dax, type = "l", xlab = "", ylab = "DAX")
lines(as.Date(row.names(DAX)), ma, type = "l")

res <- btest(dax, signal = ma_strat,
             b = n, ma = ma)

par(mfrow = c(3,1))
plot(as.Date(row.names(DAX)), dax, type = "l",
     xlab = "", ylab = "DAX")
plot(as.Date(row.names(DAX)), res$wealth, type = "l",
     xlab = "", ylab = "Equity")
plot(as.Date(row.names(DAX)), position(res), type = "s",
     xlab = "", ylab = "Position")
}
}
