\name{pricetable}
\alias{pricetable}
\alias{[.pricetable}
\title{
  Price Table
}
\description{
  Create price table
}
\usage{
pricetable(price, \dots)
}
\arguments{
%%   \item{instrument}{
%%     character: what instruments to extract from \code{price}
%% }
%%   \item{when}{
%%     The timestamps at which to extract prices. Timestamps that
%%     correspond to the rows of price are either the rownames or the
%%     values of \code{price.timestamp}. If the latter is specified, the
%%     rownames are ignored.
%% }
  \item{price}{
    a matrix
}
%%   \item{price.instrument}{
%%     instruments that accord with columns of price
%% }
%%   \item{price.timestamp}{
%%     pricestamps that accord with rows of price
%% }
%%   \item{missing}{
%%     error, NA or numeric
%% }
  \item{\dots}{
    further arguments, passed to methods
}
}
\details{

  \code{pricetable} is a helper function for extracting
  prices of particular instrument at specified dates.
  For this, it first creates a table that merges series
  passed via \dots and appends a class attribute. A \code{[}
  method then allows to extract prices. Importantly, if
  you ask for a subset of \emph{m} rows and \emph{n} columns, the
  result will be a matrix of size \emph{m} times \emph{n}, even if
  times or instruments are missing.

  \code{pricetable} is a generic function, currently
  with methods for numeric vectors (including vectors
  with a \code{\link{dim}}, aka matrices) and for
  \code{\link{zoo}} objects.

}
\value{

  a numeric matrix with class attribute \code{pricetable}

}
\references{
  Schumann, E. (2020) \emph{Portfolio Management with
    R}. \url{http://enricoschumann.net/R/packages/PMwR/}
}
\author{
  Enrico Schumann
}
\seealso{
  \code{\link{match}}
}
\examples{
## quickly creating a pricetable
pricetable(1:3)
pricetable(1:3, instrument = c("A", "B", "C"))
### ... and the same
pricetable(c(A = 1, B = 2, C = 3))


## subsetting examples
m <- 3
n <- 2
price <- array(c(1:m, 1:m + 100), dim = c(m,n))
colnames(price) <- LETTERS[1:n]
pt <- pricetable(price, timestamp = 1:m)
##   A   B
## 1 1 101
## 2 2 102
## 3 3 103

pt[ , "A"]
##   A
## 1 1
## 2 2
## 3 3

pt[ , c("X", "A", "X")]
##    X A  X
## 1 NA 1 NA
## 2 NA 2 NA
## 3 NA 3 NA

pt[ , c("X", "A", "X"), missing = 0]
##   X A X
## 1 0 1 0
## 2 0 2 0
## 3 0 3 0

pt[c(0, 1.5, 4), , missing = "locf"]
##      A   B
## 0   NA  NA
## 1.5  2 102
## 4    3 103
}
