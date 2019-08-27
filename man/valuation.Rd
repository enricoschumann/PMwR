\name{valuation}
\alias{valuation}
\alias{valuation.journal}
\alias{valuation.position}
\title{
  Valuation
}
\description{

  Valuation of financial objects: map an object into a
  quantity that is measured in a concrete (typically
  currency) unit.

}
\usage{
valuation(x, ...)

\method{valuation}{journal}(x, multiplier = 1,
          cashflow = function(x, \dots) x$amount * x$price,
          instrument = function(x, \dots) "cash",
          flip.sign = TRUE, \dots)

\method{valuation}{position}(x, price.table, multiplier = 1,
          do.sum = FALSE,
          price.unit, verbose = TRUE, do.warn = TRUE, \dots)

}
\arguments{
  \item{x}{
    an object
}
  \item{multiplier}{
    a numeric vector, typically with named elements
}
  \item{cashflow}{
    either a numeric vector or a function that takes on argument (a
    journal) and transforms it into a numeric vector
}
  \item{instrument}{
    either a character vector or a function that takes on argument (a
    journal) and transforms it into a character vector
}
  \item{flip.sign}{
    logical.  If \code{TRUE} (the default), a positive amount gets
    mapped into a negative cashflow.
}
  \item{price.table}{
    numeric
  }
  \item{do.sum}{
    logical: sum over positions
  }
  \item{price.unit}{
    a named character vector. Not implemented.
  }
  \item{verbose}{
    logical
  }
  \item{do.warn}{
    logical
  }
  \item{\dots}{
    other arguments passed to methods
}
}
\details{

  \code{valuation} is a generic function.  Its
  semantics suggest that an object (e.g. a financial
  instrument or a position) is mapped into a concrete
  quantity (such as an amount of some currency).

  The \code{\link{journal}} method transforms the
  transactions in a journal into amounts of currency
  (e.g, a sale of 100 shares of a company is
  transformed into the value of these 100 shares).

  The \code{\link{position}} method takes a position
  and returns the value (in currency units) of the
  position.

}
\value{
  depends on the object: for journals, a \code{\link{journal}}
}
\references{

  Schumann, E. (2018) \emph{Portfolio Management with R}.
  \url{http://enricoschumann.net/R/packages/PMwR/}

}
\author{
  Enrico Schumann <es@enricoschumann.net>
}
\note{
  \bold{Very experimental.}
}
\seealso{
  \code{\link{journal}}
}
\examples{
j <- journal(amount = 10, price = 2)
##    amount  price
## 1      10      2
##
## 1 transaction

valuation(j, instrument = NA)
##    amount  price
## 1     -20      1
##
## 1 transaction
}
