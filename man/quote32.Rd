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
quote32(price, sep = "(-|'|:)", warn = TRUE)
q32(price, sep = "(-|'|:)", warn = TRUE)
}
\arguments{
  \item{price}{
    numeric or character. See Details.
}
  \item{sep}{
    character: a regular expression
}
  \item{warn}{
    logical. Warn about rounding errors?
}
}
\details{

  The function is meant for pretty-printing of US
  treasury bond quotes; it provides no other
  functionality.

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
  \url{https://www.cmegroup.com/education/articles-and-reports/treasury-futures-price-rounding-conventions.html}

}
\author{
  Enrico Schumann
}
\examples{
quote32(100 + 17/32 + 0.75/32)
q32("100-172")

q32("100-272") - q32("100-270")
as.numeric(q32("100-272") - q32("100-270"))
}
