\name{pl}
\alias{pl}
\alias{.pl}
\alias{.pl_stats}
\alias{pl.data.frame}
\alias{pl.default}
\alias{pl.journal}
\alias{pl.pl}
\alias{print.pl}
\alias{as.data.frame.pl}
\title{
  Profit and Loss
}
\description{
  Compute profit and (or) loss of financial transactions.
}
\usage{
pl(amount, \dots )

\method{pl}{default}(amount, price, timestamp = NULL,
   instrument = NULL, multiplier = 1,
   multiplier.regexp = FALSE,
   along.timestamp = FALSE, approx = FALSE,
   initial.position = NULL, initial.price = NULL,
   vprice = NULL, tol = 1e-10, do.warn = TRUE,
   do.sum = FALSE, pl.only = FALSE,
   footnotes = TRUE, \dots )

\method{pl}{journal}(amount, multiplier = 1,
   multiplier.regexp = FALSE,
   along.timestamp = FALSE, approx = FALSE,
   initial.position = NULL, initial.price = NULL,
   vprice = NULL, tol = 1e-10, do.warn = TRUE, \dots )

\method{pl}{pl}(amount, \dots )

\method{print}{pl}(x, \dots, use.crayon = NULL, na.print = ".",
        footnotes = TRUE)

\method{as.data.frame}{pl}(x, \dots )

.pl(amount, price, tol = 1e-10, do.warn = TRUE)
.pl_stats(amount, price, tol = sqrt(.Machine$double.eps))

}
\arguments{

  \item{amount}{numeric or a \code{\link{journal}}}

  \item{price}{numeric}

  \item{instrument}{character or numeric (though
    typically character)}

  \item{timestamp}{An atomic vector of mode
    \code{\link{numeric}} or \code{\link{character}}.
    Timestamps should typically be sortable.}

  \item{along.timestamp}{logical; or a a vector of
    timestamps. If the latter, \code{vprice} must be
    specified as well. See the vignette
    \dQuote{Profit/Loss for Open Positions}
    (\code{pl_open_positions}) for details.  Timestamps
    must be in ascending order and will be sorted if
    they are not (and \code{vprice} will then be sorted
    as well).}

  \item{initial.position}{%
    a \code{\link{position}}
  }
  \item{initial.price}{%
    prices to evaluate initial position
  }
  \item{vprice}{%

    valuation price; a numeric vector. With several instruments, the
    prices must be named, e.g. \code{c(stock1 = 100, stock2 = 101)}.
    See Details.

  }
  \item{multiplier}{%

    numeric vector. When \code{instrument} is specified
    and the vector is named, the names will be matched
    against instruments.

  }
  \item{multiplier.regexp}{%

    logical. If \code{TRUE}, the names of
    \code{multiplier} are interpreted as regular
    expressions. See Examples.

  }
  \item{approx}{logical}
  \item{tol}{numeric: threshold to consider a position zero.}
  \item{x}{a \code{pl} object to be printed or to be coerced to a data.frame}
  \item{\dots}{further argument}
  \item{use.crayon}{logical}
  \item{na.print}{character: how to print \code{NA} values}
  \item{do.warn}{logical: issue warnings?}
  \item{do.sum}{logical: sum profit/loss across instruments?}
  \item{pl.only}{logical: if \code{TRUE}, return only
    numeric vector of profit/loss}
  \item{footnotes}{%
    logical, with default \code{TRUE}:
    collect and print notes?}
}
\details{

  Computes profit and/or loss and returns a list with
  several statistics (see Section Value, below). To get only
  the profit/loss numbers as a numeric vector, set argument
  \code{pl.only} to \code{TRUE}.

  \code{pl} is a generic function: The default input is
  vectors for amount, price, etc. Alternatively (and often
  more conveniently), the function may also be called with a
  \code{\link{journal}} or a \code{\link{data.frame}} as its
  input. For data frames, columns must be named
  \code{amount}, \code{price}, and so on, as in a
  \code{\link{journal}}.

  \code{pl} may be called in two ways: either to compute
  \emph{total profit/loss} from a list of trades, possibly
  broken down by \code{instrument} and \code{account}; or to
  compute \emph{profit/loss over time}. The latter case
  typically requires setting arguments
  \code{along.timestamp} and/or \code{vprice} (see
  Examples).  Profit/loss over time is always computed with
  time in ascending order: so if the timestamps in
  \code{along.timestamp} are not sorted, the function will
  sort them (and \code{vprice} as well).

  Using \code{vprice}: when \code{along.timestamp} is
  logical (\code{FALSE} or \code{TRUE}),
  \code{vprice} can be used to value an open
  position. For a single asset, it should be a single
  number; for several assets, it should be named
  vector, with names indicating the \code{instrument}.
  When \code{along.timestamp} is used to pass a
  custom timestamp: for a single asset, \code{vprice}
  must be a vector with the same length as
  \code{along.timestamp}; for several assets, it must
  be a numeric matrix with dimension
  \code{length(along.timestamp)} times number of
  assets.

  \code{.pl} and \code{.pl_stats} are helper functions
  that are called by \code{pl}.  \code{.pl_stats}
  requires amount and price to be sorted in time, and
  to be of length > 0.

  To use package \pkg{crayon} -- which is only sensible
  in interactive use --, either explicitly set
  \code{use.crayon} to \code{TRUE} or set an option
  \code{PMwR.use.crayon} to \code{TRUE}.


}
\value{

  For \code{pl}, an object of class \code{pl}, which is
  a list of lists: one list for each instrument. Each
  such list contains numeric vectors: \code{pl},
  \code{realised}, \code{unrealised}, \code{buy},
  \code{sell}, \code{volume}. If \code{along.timestamp}
  is not \code{\link{FALSE}}, a vector \code{timestamp}
  is also present.

  For \code{.pl}, a numeric vector with four elements:
  profit/loss in units of the instrument, sum of
  absolute amounts, average buy price, average sell
  price. For zero-length vector, the function evaluates to
  \code{c(0, 0, NaN, NaN)}.

  For \code{.pl_stats}, a list of two elements:
  the average entry-price, and the realized profit/loss.
  profit/loss in units of the instrument, sum of
  absolute amounts, average buy price, average sell
  price. For zero-length vector, the function evaluates to
  \code{c(0, 0, NaN, NaN)}.

}
\references{

  Schumann, E. (2023) \emph{Portfolio Management with R}.
  \url{https://enricoschumann.net/PMwR/}; in particular
  \url{https://enricoschumann.net/R/packages/PMwR/manual/PMwR.html#profit-and-loss}

}
\author{
  Enrico Schumann <es@enricoschumann.net>
}
\seealso{
  \code{\link{btest}}, \code{\link{returns}}
}
\examples{
J <- journal(timestamp = c(  1,   2,   3),
             amount    = c(  1,   1,  -2),
             price     = c(100, 102, 101))
pl(J)

pl(amount = c(  1,   1,  -2),
   price  = c(100, 102, 101))  ## without a 'journal'


J <- journal(timestamp  = c(  1,   2,   3,   1,   2,   3),
             amount     = c(  1,   1,  -2,   1,   1,  -2),
             price      = c(100, 102, 101, 100, 102, 105),
             instrument = c(rep("Bond A", 3), rep("Bond B", 3)))

pl(J)
## Bond A
##   P/L total       0
##   average buy   101
##   average sell  101
##   cum. volume     4
##
## Bond B
##   P/L total       8
##   average buy   101
##   average sell  105
##   cum. volume     4
##
## 'P/L total' is in units of instrument;
## 'volume' is sum of /absolute/ amounts.

as.data.frame(pl(J))  ## a single data.frame
##        pl buy sell volume
## Bond A  0 101  101      4
## Bond B  8 101  105      4

lapply(pl(J), as.data.frame)  ## => a list of data.frames
## $`Bond A`
##   pl realised unrealised buy sell volume
## 1  0       NA         NA 101  101      4
##
## $`Bond B`
##   pl realised unrealised buy sell volume
## 1  8       NA         NA 101  105      4

pl(pl(J))  ## P/L as a numeric vector
## Bond A Bond B
##      0      8




## Example for 'vprice'
instrument  <- c(rep("Bond A", 2), rep("Bond B", 2))
amount <- c(1, -2, 2, -1)
price <- c(100, 101, 100, 105)

## ... no p/l because positions not closed:
pl(amount, price, instrument = instrument, do.warn = FALSE)

## ... but with vprice specified, p/l is computed:
pl(amount, price, instrument = instrument,
   vprice = c("Bond A" = 103, "Bond B" = 100))

### ... and is, except for volume, the same as here:
instrument  <- c(rep("Bond A", 3), rep("Bond B", 3))
amount <- c(1, -2, 1, 2, -1, -1)
price <- c(100, 101, 103, 100, 105, 100)
pl(amount, price, instrument = instrument)



## p/l over time: example for 'along.timestamp' and 'vprice'
j <- journal(amount = c(1, -1),
             price = c(100, 101),
             timestamp  = as.Date(c("2017-07-05", "2017-07-06")))
pl(j)

pl(j,
   along.timestamp = TRUE)

pl(j,
   along.timestamp = seq(from = as.Date("2017-07-04"),
                         to = as.Date("2017-07-07"),
                         by = "1 day"),
   vprice = 101:104)



## Example for 'multiplier'
jnl <- read.table(text =
"instrument, price, amount
 FGBL MAR 16, 165.20,  1
 FGBL MAR 16, 165.37, -1
 FGBL JUN 16, 164.12,  1
 FGBL JUN 16, 164.13, -1
 FESX JUN 16,   2910,  5
 FESX JUN 16,   2905, -5",
header = TRUE, stringsAsFactors = FALSE, sep = ",")


jnl <- as.journal(jnl)
pl(jnl,  multiplier.regexp = TRUE, ## regexp matching is case sensitive
   multiplier = c("FGBL" = 1000, "FESX" = 10))



## use package 'crayon'
\dontrun{
## on Windows, you may also need 'options(crayon.enabled = TRUE)'
options(PMwR.use.crayon = FALSE)
pl(amount = c(1, -1), price = c(1, 2))
options(PMwR.use.crayon = TRUE)
pl(amount = c(1, -1), price = c(1, 2))
}

}
