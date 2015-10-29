\name{pricetable}
\alias{pricetable}
\title{
  Price table
}
\description{
  Create price table
}
\usage{
pricetable(instrument, when, price,
           price.instrument = NULL, price.timestamp = NULL,
           missing = "error")
}
\arguments{
  \item{instrument}{
    character
}
  \item{when}{
    timestamps
}
  \item{price}{
    a matrix
}
  \item{price.instrument}{
    instruments that accord with columns of price
}
  \item{price.timestamp}{
    pricestamps that accord with rows of price
}
  \item{missing}{
    error, NA or numeric
}
}
\details{
  Experimental: interface may change; may become generic.
}
\value{
  A matrix with class attribute pricetable
}
\references{
  Schumann, E. (2015) \emph{Portfolio Management with
    R}. \url{http://enricoschumann.net/R/packages/PMwR/}
}
\author{
  Enrico Schumann
}
\seealso{
  \code{\link{match}}
}
\examples{
m <- 20
n <- 8
price <- array(rnorm(m*n), dim = c(m,n))
colnames(price) <- letters[1:n]
rownames(price) <- 1:m

when <- c(0,1.2,3,3,20)
instrument <- c("a", "c", "x", "z_1", "z_2")

pricetable(instrument, when, price, missing = NA)
pricetable(instrument, when, price, missing = 1)

}
