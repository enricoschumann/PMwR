\name{PMwR-internal}
\alias{PMwR-internal}
\alias{makeHHMMSS}
\alias{wait}
\alias{first}
\alias{last}
\alias{matchOrNext}
\alias{matchOrPrevious}
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

letter2month(s, instrument = "option")

matchOrNext(x, y)
matchOrPrevious(x, y)
}
\details{

  \code{makeHHMMSS} transforms the character \code{x} into a
  character vector that resembles a format \code{HHMMSS}. 

  If the argument to \code{wait} is a single number, the function will
  call \code{\link{Sys.sleep}} with this number. If \code{x} is a
  vector, a uniform random variate between \code{min(x)} and
  \code{max(x)} is passed to \code{\link{Sys.sleep}}.  

  \code{letter2month} converts a string to an option type and month (eg,
  \code{"A"} means \code{"Call January"}) or to an expiry month (with
  \code{instrument} set to \code{future}).

  \code{matchOrNext} returns the position of \code{x} in \code{y}, if
  there is a match.  If not, it reports the next position. \code{y}
  must be sorted.
  
}
\author{
  Enrico Schumann
}
\examples{
dates <- seq(as.Date("2012-01-01"), as.Date("2013-02-05"), by = "1 day")
PMwR:::last(dates,  by = format(dates, "\%Y\%m"))
PMwR:::first(dates, by = format(dates, "\%Y\%m"))

x <- c(2, 2.5)
PMwR:::matchOrNext(x, 1:5)
PMwR:::matchOrPrevious(x, 1:5)

PMwR:::matchOrNext(c(0,6), 1:5)
PMwR:::matchOrPrevious(c(0,6), 1:5)
}
\keyword{internal}
