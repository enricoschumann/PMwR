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
      trade.at.open = TRUE, tol = 1e-5, tol.p = NA,
      Globals = list(),
      prices0 = NULL,
      include.data = FALSE, include.timestamp = TRUE,
      timestamp, instrument,
      progressBar = FALSE,
      variations, variations.settings, replications)
}
\arguments{
  \item{prices}{

    For a single asset, a matrix of prices with four
    columns: open, high, low and close. For \code{n}
    assets, a list of length four: \code{prices[[1]]}
    is then a matrix with \code{n} columns containing
    the open prices for the assets; \code{prices[[2]]}
    is a matrix with the high prices, and so on. If
    only close prices are used, then for a single asset
    either a matrix of one column or a numeric vector;
    for multiple assets a list of length one,
    containing the matrix of close prices. For example,
    with 100 close prices of 5 assets, the prices
    should be arranged in a matrix \code{p} of size 100
    times 5; and \code{prices = list(p)}.

    The series in \code{prices} are used both as
    transaction prices and for valuing open
    positions. If signals are to be based on other
    series, such other series should be passed via the
    \dots argument.

    Prices must be ordered by time (though the
    timestamps need not be provided).

  }
  \item{signal}{

    A function that evaluates to the position in units
    of the instruments suggested by the trading
    rule. If \code{convert.weights} is \code{TRUE},
    \code{signal} should return the suggested position
    as weights (which need not sum to 1).  If \code{signal}
    returns \code{NULL}, the current position is kept.
    See Details.

  }
  \item{do.signal}{

    Logical or numeric vector, a function that
    evaluates to \code{TRUE} or \code{FALSE}, or a
    string.

    When a logical vector, its length must match the
    number of observations in prices: \code{do.signal}
    then corresponds to the rows in \code{prices} at
    which a signal is computed. Alternatively, these
    rows may also be specified as integers.  If a
    length-one \code{TRUE} or \code{FALSE}, the value
    is recycled to match the number of observations in
    prices.  Default is \code{TRUE}: a signal is then
    computed in every period.

    \code{do.signal} may also be the string
    \dQuote{firstofmonth}, \dQuote{lastofmonth},
    \dQuote{firstofquarter} or \dQuote{lastofquarter};
    in these cases, \code{timestamp} needs to specified
    and must be coercable to \code{\link{Date}}.

  }
  \item{do.rebalance}{

    Same as \code{do.signal}. Can also be the string
    \code{"do.signal"}, in which case the value of
    \code{do.signal} is copied.

  }
  \item{print.info}{

    A function or \code{NULL}. If \code{NULL}, nothing
    is printed.

  }
  \item{cashflow}{

    A function or \code{NULL} (default).

  }
  \item{b}{

    burn-in (an integer). Defaults to 1. This may also
    be a length-one timestamp of the same class as
    \code{timestamp}, in which case the data up to (and
    including) \code{b} are skipped.

  }
  \item{fraction}{

    amount of rebalancing to be done: a scalar between
    0 and 1

  }
  \item{initial.position}{

    a numeric vector: initial portfolio in units of
    instruments.  If supplied, this will also be the
    initial suggested position.

  }
  \item{initial.cash}{

    a numeric vector of length 1. Defaults to 0.

  }
  \item{final.position}{

    logical

  }
  \item{tc}{

    transaction costs as a fraction of turnover (e.g.,
    0.001 means 0.1\%). May also be a function that
    evaluates to such a fraction. More-complex
    computations may be specified with
    argument \code{cashflow}.

  }
  \item{\dots}{

    other named arguments. All functions (signal,
    do.signal, do.rebalance, print.info, cashflow) will
    have access to these arguments. See Details for
    reserved argument names.

  }
  \item{add}{

    Default is \code{FALSE}. \code{TRUE} is \strong{not
    implemented} -- but would mean that \code{signal}
    should evaluate to \emph{changes} in position,
    i.e. orders.

  }
  \item{lag}{

    default is 1

  }
  \item{convert.weights}{

    Default is \code{FALSE}.  If \code{TRUE}, the value
    of signal will be considered a weight vector and
    automatically translated into (fractional) position
    sizes.

  }
  \item{trade.at.open}{

    A logical vector of length one; default is \code{TRUE}.

  }
  \item{tol}{

    A numeric vector of length one: only rebalance if
    the maximum absolute suggested change for at least
    one position is greater than \code{tol}. Default is
    0.00001 (which practically means always rebalance).

  }
  \item{tol.p}{

    A numeric vector of length one: only rebalance
    those positions for which the relative suggested
    change is greater than \code{tol.p}. Default is
    \code{\link{NA}}: always rebalance.

  }
  \item{Globals}{

    A \code{list} of named elements. See Details.

  }
  \item{prices0}{

    A numeric vector (default is \code{NULL}). Only
    used if \code{b} is 0 and an initial portfolio
    (\code{initial.position}) is specified.

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
  \item{variations}{
     a list. See Details.
  }

  \item{variations.settings}{
    a list. See Details.
  }

  \item{replications}{

    an integer. If set, the function returns a list of
    \code{btest} objects.  Each \code{btest} has an
    attribute \code{replication}, which records the
    replication number.

  }
}
\details{

  The function provides infrastructure for testing
  trading rules. Essentially, \code{btest} does
  accounting: keep track of transactions and positions,
  value open positions, etc. The ingredients are price
  time-series (single series or \acronym{OHLC}
  bars), which need not be equally spaced; and several
  functions that map these series and other pieces of
  information into positions.

  \subsection{How \code{btest} works}{

    \code{btest} runs a loop from \code{b + 1} to
    \code{NROW(prices)}. In iteration \code{t}, a
    \code{signal} can be computed based on information
    from periods prior to \code{t}. Trading then takes
    place at the opening price of \code{t}.  For
    slow-to-compute signals this is reasonable if there
    is a time lag between close and open. For daily
    prices, for instance, signals could be computed
    overnight. For higher frequencies, such as every
    minute, the signal function should be fast to
    compute. Alternatively, it may be better to use a
    larger time offset (i.e. use a longer time lag) and
    to trade at the close of \code{t} by setting
    argument \code{trade.at.open} to \code{FALSE}.

    If no \acronym{OHLC} bars are available, a single
    series per asset (assumed to be close prices) can
    be used.

    The trade logic needs to be coded in the function
    \code{signal}. Arguments to that function must be
    named and need to be passed with \code{...}.
    Certain names are reserved and cannot be used as
    arguments: \code{Open}, \code{High}, \code{Low},
    \code{Close}, \code{Wealth}, \code{Cash},
    \code{Time}, \code{Timestamp}, \code{Portfolio},
    \code{SuggestedPortfolio}, \code{Globals}. Further
    reserved names may be added in the future:
    \strong{it is suggested to not start an argument
    name with capital letter}.

    The function \code{signal} must evaluate to the
    target position in units of the instruments. To
    work with weights, set \code{convert.weights} to
    \code{TRUE}, and \code{btest} will translate the
    weights into positions.

  }

  \subsection{Accessing data}{

    Within \code{signal} (and also other function
    arguments, such as \code{do.signal}), you can
    access data via special functions such as
    \code{Close}. These are automatically added as
    arguments to \code{signal}. Currently, the
    following functions are available: \code{Open},
    \code{High}, \code{Low}, \code{Close},
    \code{Wealth}, \code{Cash}, \code{Time},
    \code{Timestamp}, \code{Portfolio},
    \code{SuggestedPortfolio}, \code{Globals}.
    \code{Globals} is special: it is an
    \code{\link{environment}}, which can be used to
    persistently store data during the run of
    \code{btest}. Use the argument \code{Globals} to
    add initial objects. See the Examples below and the
    manual.

    Additional functions may be added to \code{btest}
    in the future. The names of those functions will
    always be in title case. Hence, it is recommended
    to not use argument names for \code{signal},
    etc. that start with a capital letter.

  }

  \subsection{Replications and variations}{

    \code{btest} allows to run backtests in
    parallel. See the examples at
    \url{http://enricoschumann.net/notes/parallel-backtests.html}.

    The argument \code{variations.settings} is a list with the
    following defaults:
    \describe{
      \item{\code{method}}{character: supported are
        \code{"loop"}, \code{"parallel"} (or \code{"snow"})
        and \code{"multicore"}}
      \item{\code{load.balancing}}{logical}
      \item{\code{cores}}{numeric}
    }

  }
}
\value{
  A list with class attribute \code{btest}. The list comprises:
  \item{\code{position}}{actual portfolio holdings}
  \item{\code{suggested.position}}{suggested holdings}
  \item{\code{cash}}{cash}
  \item{\code{wealth}}{time-series of total portfolio
    value (aka equity curve)}
  \item{\code{cum.tc}}{transaction costs}
  \item{\code{journal}}{\code{\link{journal}} of trades}
  \item{\code{initial.wealth}}{initial wealth}
  \item{\code{b}}{burn-in}
  \item{\code{final.position}}{final position if \code{final.position} is
    \code{TRUE}; otherwise \code{\link{NA}}}
  \item{\code{Globals}}{environment \code{Globals}}

  When \code{include.timestamp} is \code{TRUE}, the
  timestamp is included. If no \code{timestamp} was
  specified, integers \code{1, 2, ...} are used
  instead.

  When \code{include.data} is \code{TRUE}, essentially
  all information (prices, instrument, the
  actual \code{call} and functions \code{signal} etc.)
  are stored in the list as well.

}
\references{

  Schumann, E. (2019) \emph{Portfolio Management
    with R}. \url{http://enricoschumann.net/PMwR/};
  in particular the chapter on backtesting:
  \url{http://enricoschumann.net/R/packages/PMwR/manual/PMwR.html#backtesting}

  Schumann, E. (2018) \emph{Backtesting}. \url{https://ssrn.com/abstract=3374195}

}
\author{
  Enrico Schumann \email{es@enricoschumann.net}
}
\examples{
## For more examples, please see the Manual
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

res <- btest(prices = dax,
             signal = ma_strat,
             b = n, ma = ma)

par(mfrow = c(3, 1))
plot(as.Date(row.names(DAX)), dax, type = "l",
     xlab = "", ylab = "DAX")
plot(as.Date(row.names(DAX)), res$wealth, type = "l",
     xlab = "", ylab = "Equity")
plot(as.Date(row.names(DAX)), position(res), type = "s",
     xlab = "", ylab = "Position")
}
}
