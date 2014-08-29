\name{scale1}
\alias{scale1}
\alias{scale1.default}
\alias{scale1.zoo}
\title{
  Scale Time Series
}
\description{
  Scale time series.
}
\usage{
scale1(x, \dots)

\method{scale1}{default}(x, \dots, when = "first.complete", level = 1,
                         centre = FALSE, scale = FALSE)

\method{scale1}{zoo}(x, \dots, when = "first.complete", level = 1,
                     centre = FALSE, scale = FALSE)
}
\arguments{
  \item{x}{
    a time series
}
  \item{when}{
    origin: either character or numeric (integer)
}
  \item{level}{
    numeric
}
  \item{centre}{
    logical
}
  \item{scale}{
    logical or numeric
}
  \item{\dots}{
    other arguments passed to methods
}
}
\details{
  This is a generic function
}
\value{
An object of the same type as \code{x}.
}
\references{
  Enrico Schumann -- PMwR
}
\author{
  Enrico Schumann
}
\note{
  Experimental: interface may change.
}
\seealso{
  \code{\link{scale}}
}
\examples{
1+1
}
