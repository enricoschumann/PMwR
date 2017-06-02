\name{pl}
\alias{pl}
\alias{.pl}
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
pl(amount, \dots)

\method{pl}{default}(amount, price, timestamp = NULL,
                     instrument = NULL, multiplier = 1,
                     multiplier.regexp = FALSE,
                     along.timestamp = FALSE, approx = FALSE,
                     initial.position = NULL, initial.price = NULL,
                     vprice = NULL, tol = 1e-10, do.warn = TRUE, \dots)

\method{pl}{journal}(amount, multiplier = 1,
                     multiplier.regexp = FALSE,
                     along.timestamp = FALSE, approx = FALSE,
                     initial.position = NULL, initial.price = NULL,
                     vprice = NULL, tol = 1e-10, do.warn = TRUE, \dots)

\method{pl}{pl}(amount, \dots)

.pl(amount, price, tol = 1e-10, do.warn = TRUE)

\method{print}{pl}(x, \dots, use.crayon = NULL, na.print = ".")

\method{as.data.frame}{pl}(x, \dots)
}
\arguments{
  \item{amount}{
    notionals
  }
  \item{price}{
    a vector of prices
  }
  \item{instrument}{
    a character vector
  }
  \item{timestamp}{
    a vector. 
  }
  \item{along.timestamp}{
    a logical
  }
  \item{initial.position}{
    a \code{\link{position}}.
  }
  \item{initial.price}{
    prices to evaluate initial position.
  }
  \item{vprice}{

    numeric vector. With several instruments, the prices should be
    named, e.g. \code{c(stock1 = 100, stock2 = 101)}.

  }
  \item{multiplier}{

    numeric vector. When \code{instrument} is specified and the vector
    is named, the names will be matched against instruments.

  }
  \item{multiplier.regexp}{

    logical. If \code{TRUE}, the names of \code{multiplier} are
    interpreted as regular expressions. See Examples.

  }
  \item{approx}{logical}
  \item{tol}{numeric: threshold to consider a position zero.}
  \item{x}{a pl object to be printed or to be coerced to a data.frame}
  \item{\dots}{further argument}
  \item{use.crayon}{logical}
  \item{na.print}{character: how to print \code{NA} values}
  \item{do.warn}{logical: issue warnings?}
}
\details{

  Computes profit and/or loss and returns a list with
  several statistics (see Section Value, below). To get
  only the profit/loss numbers as a numeric vector,
  call \code{pl} twice (\code{pl(pl(...))}).

  
  
  To use package crayon -- which is only sensible in
   interactive use --, either explicitly set it to
   \code{TRUE} or set an option \code{PMwR.use.crayon}
   to \code{TRUE}.

  
}
\value{

  For \code{pl}, an object of class \code{pl}, which is
  a list of lists: one list for each instrument. Each
  such list contains numeric vectors: \code{pl},
  \code{realised}, \code{unrealised}, \code{buy},
  \code{sell}, \code{volume}.

  For \code{.pl}, a numeric vector with four elements:
  profit/loss in units of the instrument, sum of
  absolute amounts, average buy price, average sell
  price.
  
}
\references{
Schumann, E. (2017) \emph{Portfolio Management
  with R}. \url{http://enricoschumann.net/R/packages/PMwR/manual/PMwR.html}

} 
\author{
Enrico Schumann <es@enricoschumann.net>
}
\seealso{
  \code{\link{btest}},   \code{\link{returns}}
}
\examples{
J <- journal(timestamp = c(  1,   2,   3),
             amount    = c(  1,   1,  -2),
             price     = c(100, 102, 101))
pl(J)
pl(c(1, 1, -2), c(100,102, 101))  ## without a 'journal'


J <- journal(timestamp  = c(  1,   2,   3,   1,   2,   3),
             amount     = c(  1,   1,  -2,   1,   1,  -2),
             price      = c(100, 102, 101, 100, 102, 105),
             instrument = c(rep("Bond A", 3), rep("Bond B", 3)))
pl(J)
as.data.frame(pl(J))
pl(pl(J))  ## P/L as a numeric vector


## Example for 'vprice'
instrument  <- c(rep("Bond A", 2), rep("Bond B", 2))
amount <- c(1, -2, 2, -1)
price <- c(100, 101, 100, 105)

## ... no p/l + warning because positions not closed:
pl(amount, price, instrument = instrument) 

## ... but with vprice specified, p/l is computed:
pl(amount, price, instrument = instrument, ## p/l + no warning
   vprice = c("Bond A" = 103, "Bond B" = 100)) 

### ... and is, except for volume, the same as here:
instrument  <- c(rep("Bond A", 3), rep("Bond B", 3))
amount <- c(1, -2, 1, 2, -1, -1)
price <- c(100, 101, 103, 100, 105, 100)
pl(amount, price, instrument = instrument) ## no p/l + warning


## Example for 'multiplier
jnl <- read.table(textConnection(
"instrument, price, amount
 FGBL MAR 16, 165.20,  1
 FGBL MAR 16, 165.37, -1
 FGBL JUN 16, 164.12,  1
 FGBL JUN 16, 164.13, -1
 FESX JUN 16,   2910,  5
 FESX JUN 16,   2905, -5"
), header = TRUE, stringsAsFactors = FALSE, sep = ",")

jnl <- as.journal(jnl)
pl(jnl,  multiplier.regexp = TRUE, ## regexp matching is case sensitive
   multiplier = c("FGBL" = 1000, "FESX" = 10))


## use package 'crayon'
\dontrun{
options(PMwR.use.crayon = FALSE)
pl(amount = c(1, -1), price = c(1, 2))
options(PMwR.use.crayon = TRUE)
pl(amount = c(1, -1), price = c(1, 2))
}

}
