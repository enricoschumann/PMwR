\name{quote32}
\alias{quote32}
\alias{q32}
\title{
  Treasury Quotes with 1/32nds of Point
}
\description{
  Print treasury quotes with 1/32nds of points.  
}
\usage{
quote32(price, sep = "-")
q32(price, sep = "-")
}
\arguments{
  \item{price}{
    numeric or character. See Details.
}
  \item{sep}{
    character
}
}
\details{
  
  If \code{price} is \code{numeric}, it is interpreted as a quote in
  decimal notation and \sQuote{translated} into a price quoted in
  fractions of a point.

  If \code{price} is \code{character}, it is interpreted as a quote in
  fractional notation.

  \code{q32} is a short-hand for \code{quote32}.
  
}
\value{
  A numeric vector of class \code{quote32}.
}
\references{

  CME Group (2015). \emph{Treasury Futures Price Rounding Conventions.}  
  \url{http://www.cmegroup.com/education/treasury-futures-price-rounding-conventions.html}

}
\author{
  Enrico Schumann
}
\examples{
quote32(100 + 17/32 + 0.75/32)
q32("100-272")


}
