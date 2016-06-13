\name{quote32}
\alias{quote32}
\title{
  Treasury Quotes with 1/32nds of Point
}
\description{
  Print treasury quotes with 1/32nds of points.  
}
\usage{
quote32(price, sep = "-")
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
  
  
}
\value{
  A numeric vector of class \code{quote32}.
}
\references{
  CME 
}
\author{
  Enrico Schumann
}
\examples{
quote32(100 + 17/32 + 0.75/32)

}
