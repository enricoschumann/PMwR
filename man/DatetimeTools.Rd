\name{DatetimeTools}
\alias{previousBusinessDay}
\alias{nextBusinessDay}
\alias{isWeekend}
\alias{firstOfMonth}
\alias{endOfMonth}
\alias{endOfPreviousMonth}
\alias{setDayOfMonth}
\alias{timegrid}
\alias{roundPOSIXt}
\title{
  Handling and Manipulating Dates and Times
}
\description{
    Functions that help to manipulate vectors that inherits from class
  \code{POSIXt}. If times are returned, they are typically of class POSIXct.

}
\usage{
timegrid(from, to, interval, excludeWeekends = TRUE, holidays = NULL,
         fromHHMMSS = "080000", toHHMMSS = "220000")
roundPOSIXt(t, interval)

previousBusinessDay(x, holidays = NULL)
nextBusinessDay(x, holidays = NULL)
isWeekend(x)
firstOfMonth(x)
endOfMonth(x)
endOfPreviousMonth(x)
setDayOfMonth(x)
}
\arguments{

  \item{from}{
    POSIXt
  }
  \item{to}{
    POSIXt
  }
  \item{interval}{
    a character string like \dQuote{num units}, in which \code{num} is a
    number, and \code{units} is sec, min, hour or day
  }
  \item{excludeWeekends}{
    logical: default is \code{TRUE}
  }
  \item{fromHHMMSS}{
    a character vector of length one like \dQuote{HHMMSS}
  }
  \item{toHHMMSS}{
    a character vector of length one like \dQuote{HHMMSS}
  }
  \item{holidays}{
    a vector of class \code{\link{Date}} or a character vector in a format that
    is understood by \code{\link{as.Date}}
  }
  \item{t}{
    a vector that inherits from class \code{POSIXt}
  }
  \item{x}{
    a vector of class \code{Date}
  }
}
\details{
  Some of these functions are currently horribly inefficient, and their
  interfaces will likely change.

  \code{roundPOSIXt} rounds to down to the specified interval.

}
\value{
  Date in, date out.
  A vector of class \code{POSIXct}.

}
\references{
  B.D. Ripley and K. Hornik. \emph{Date-Time Classes}. R-News,
  \strong{1}(2):8--12, 2001.

}
\author{
  Enrico Schumann
}
\seealso{
\code{\link{DateTimeClasses}}
}
\examples{

from <- as.POSIXct("2012-04-30 08:00:00")
to   <- as.POSIXct("2012-05-04 22:00:00")
res  <- timegrid(from, to, interval = "1 hour",
                 holidays = as.Date("2012-05-01"))

times <- as.POSIXct("2012-03-24 22:00:00") + 1:1e2
roundPOSIXt(times, "2 min")

previousBusinessDay(as.Date("2012-06-18"))
}
