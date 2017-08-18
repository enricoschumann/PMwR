\name{pricetable}
\alias{pricetable}
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
  
  Experimental: interface may change.

  \code{pricetable} is a helper function for extracting
  prices of particular instrument at specified dates.
  For this, it first creates a table that merges series
  passed via \dots and appends a class attribute. A \code{[}
  method then allows to extract prices. Importantly, if
  you ask for a subset of m rows and n columns, the
  result will be a matrix of size m times n, even if
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
  Schumann, E. (2017) \emph{Portfolio Management with
    R}. \url{http://enricoschumann.net/R/packages/PMwR/}
}
\author{
  Enrico Schumann
}
\seealso{
  \code{\link{match}}
}
\examples{
\dontrun{
m <- 20
n <- 8
price <- array(rnorm(m*n), dim = c(m,n))
colnames(price) <- letters[1:n]
rownames(price) <- 1:m

when <- c(0,1.2,3,3,20)
instrument <- c("a", "c", "x", "z_1", "z_2")

pricetable(price)[,"a"]
pricetable(price)[,"a"]

pricetable(instrument, when, price, missing = 1)

pricetable(c("a", "c"), when, price, missing = 1)
pricetable(c("c", "c"), when, price, missing = 1)
pricetable(c("c", "a"), when, price, missing = 1)
}
}
