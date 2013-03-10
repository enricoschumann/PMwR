\name{PMwR-internal}
\alias{PMwR-internal}
\alias{makeHHMMSS}
\alias{wait}
\alias{first}
\alias{last}
\title{Internal functions in the PMwR package}
\description{
  Several internal functions, not exported from the \pkg{PMwR} name space.
}
\usage{
makeHHMMSS(x, label = "time specification (HHMMSS)")
wait(x)
first(x, by, index = FALSE)
last(x, by, index = FALSE)
}
\details{

  \code{makeHHMMSS} transforms the character \code{x} into a
  character vector that resembles a format \code{HHMMSS}. 

  If the argument to \code{wait} is a single number, the function will
  call \code{\link{Sys.sleep}} with this number. If \code{x} is a
  vector, a uniform random variate between \code{min(x)} and
  \code{max(x)} is passed to \code{\link{Sys.sleep}}.

  
}
\author{
  Enrico Schumann
}
\examples{
dates <- seq(as.Date("2012-01-01"), as.Date("2013-02-05"), by = "1 day")
last(dates,  by = format(dates, "%Y%m"))
first(dates, by = format(dates, "%Y%m"))
}
\keyword{internal}
