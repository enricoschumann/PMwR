\name{PMwR-internal}
\alias{PMwR-internal}
\alias{makeHHMMSS}
\alias{wait}
\alias{first}
\alias{last}
\title{Internal (non-exported) functions in the PMwR package}
\description{

  Several internal functions, not exported from the \pkg{PMwR} name
  space.
  
}
\usage{
makeHHMMSS(x, label = "time specification (HHMMSS)")

wait(x)

first(x, by, index = FALSE)
last(x, by, index = FALSE)

divAdjust(x, t, div, backward = TRUE, additive = FALSE)

letter2month(s)
}
\details{

  \code{makeHHMMSS} transforms the character \code{x} into a
  character vector that resembles a format \code{HHMMSS}. 

  If the argument to \code{wait} is a single number, the function will
  call \code{\link{Sys.sleep}} with this number. If \code{x} is a
  vector, a uniform random variate between \code{min(x)} and
  \code{max(x)} is passed to \code{\link{Sys.sleep}}.  

  \code{letter2month} converts a string to an option type and month (eg,
  \code{"A"} means \code{"Call January"}).
  
}
\author{
  Enrico Schumann
}
\examples{
dates <- seq(as.Date("2012-01-01"), as.Date("2013-02-05"), by = "1 day")
PMwR:::last(dates,  by = format(dates, "\%Y\%m"))
PMwR:::first(dates, by = format(dates, "\%Y\%m"))

x <- c(9.777, 10.04, 9.207, 9.406)
div <- 0.7
t <- 3

PMwR:::divAdjust(x, t, div)
PMwR:::divAdjust(x, t, div, FALSE)

}
\keyword{internal}
