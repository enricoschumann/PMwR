\name{DatetimeTools}
\alias{dayOfMonth<-}
\alias{dayOfMonth}
\alias{endOfMonth}
\alias{endOfPreviousMonth}
\alias{firstOfMonth}
\alias{isWeekend}
\alias{lastWeekday}
\alias{nextBusinessDay}
\alias{previousBusinessDay}
\alias{roundPOSIXt}
\alias{timegrid}
\title{
  Handling and Manipulating Dates and Times
}
\description{
  
  Functions for manipulating vectors that inherits from class
  \code{POSIXt} or \code{Date}. If times are returned, they are
  typically of class POSIXct.

}
\usage{
timegrid(from, to, interval, excludeWeekends = TRUE, holidays = NULL,
         fromHHMMSS = "080000", toHHMMSS = "220000")
roundPOSIXt(t, interval)

previousBusinessDay(x, holidays = NULL)
nextBusinessDay(x, holidays = NULL)
isWeekend(x)
firstOfMonth(x)
endOfMonth(x, shift = 0L)
endOfPreviousMonth(x)
dayOfMonth(x)
dayOfMonth(x) <- value

}
\arguments{

  \item{from}{
    a vector of length one that inherits from class \code{\link{POSIXt}}
  }
  \item{to}{
    a vector of length one that inherits from class \code{\link{POSIXt}}
  }
  \item{interval}{
    
    A character string like \dQuote{\code{num} \code{units}}, in which
    \code{num} is a number, and \code{units} is \code{sec}, \code{min},
    \code{hour} or \code{day}.

  }
  \item{excludeWeekends}{
    logical; default is \code{TRUE}
  }
  \item{fromHHMMSS}{
    
    A character vector of length one like
    \dQuote{\code{HHMMSS}}. Times-of-day earlier than \code{HHMMSS} are
    excluded from the grid.

  }
  \item{toHHMMSS}{

    A character vector of length one like
    \dQuote{\code{HHMMSS}}. Times-of-day later than \code{HHMMSS} are
    excluded from the grid.

  }
  \item{holidays}{

    A vector of class \code{\link{Date}}, or a character vector in a format that
    is understood by \code{\link{as.Date}}, or anything that can be
    coerced to class \code{Date} by \code{\link{as.Date}} (eg, \code{\link{POSIXt}}).

  }
  \item{t}{
    a vector of length one that inherits from class \code{\link{POSIXt}}
  }
  \item{x}{
    a vector of class \code{Date}
  }
  \item{value}{
    a vector of integers
  }
  \item{shift}{
    a vector of integers
  }
}
\details{

  Some of these functions are currently quite inefficient, and their
  interfaces may change.

  \code{previousBusinessDay} takes a \code{\link{Date}} \code{x} and
  returns the last non-weekend day before.

  \code{endOfMonth} returns the last calendar day of a given month. If
  \code{shift} is positive, then shift \code{months} into the future; if
  negative, the end of previous months. (\code{endOfMonth(x, -1)} is
  equivalent to \code{endOfPreviousMonth(x)}.
  
  \code{roundPOSIXt} rounds to down to the specified interval.

}
\value{
  
  Vectors of class \code{Date} or \code{POSIXct}; \code{isWeekend}
  returns \code{TRUE} or \code{FALSE}.

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
