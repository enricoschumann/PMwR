\name{plotTradingHours}
\alias{plotTradingHours}
\title{
  Plot Time Series during Trading Hours
}
\description{
  Plot a time series after removing weekends and specific times of the day.
}
\usage{
plotTradingHours(x, t = NULL,
                 interval = "5 min", labels = "hours", label.format = NULL,
                 excludeWeekends = TRUE, holidays = NULL,
                 fromHHMMSS = "000000", toHHMMSS = "240000",
                 do.plotAxis = TRUE,
                 ...,
                 from = NULL, to = NULL,
                 do.plot = TRUE,
                 axis1.par = list())
}
\arguments{
  \item{x}{
    A numeric vector. Can also be of class \code{zoo}.
  }
  \item{t}{    
    A vector that inherits from class \code{POSIXt}. If \code{x}
    inherits from class \code{zoo}, then \code{index(x)} is used (and
    any supplied value for \code{t} is ignored).
  }
  \item{interval}{
    A character string like \dQuote{num units}, in which \code{num} is a
    number, and \code{units} is \dQuote{sec}, \dQuote{min},
    \dQuote{hour} or \dQuote{day}. The space between num and
    units is mandatory.
  }
  \item{labels}{
    A character vector of length one, determining the grid for
    \code{plotTradingHours}: can be \dQuote{hour}, \dQuote{day},
    \dQuote{dayhour} or \dQuote{month}.
  }
  \item{label.format}{
    See \code{\link{strftime}}.
  }
  \item{excludeWeekends}{
    logical: default is \code{TRUE}
  }
  \item{holidays}{
    a vector of class \code{\link{Date}} or a character vector in a format that
    is understood by \code{\link{as.Date}}. \strong{Currently not supported.}
  }
  \item{fromHHMMSS}{
    a character vector of length one in format \dQuote{HHMMSS}
  }
  \item{toHHMMSS}{
    a character vector of length one in format \dQuote{HHMMSS}
  }
  \item{do.plotAxis}{
    logical. Should \code{axis(1)} be plotted? Default is \code{TRUE}.
  }
  \item{\dots}{
    parameters passed to \code{\link{plot}} (and typically \code{\link{par}})
  }
  \item{from}{
    POSIXct: start plot at (if not specified, plot starts at first data point)
  }
  \item{to}{
    POSIXct: end plot at (if not specified, plot end at last data point)
  }
  \item{do.plot}{
    logical. Should anything be plotted? Default is \code{TRUE}. If
    \code{FALSE}, the function returns a list of points.
  }
  \item{axis1.par}{
    a list of named elements
  }
}
\details{

  This function is still very experimental. In in particular, it is
  recommended that arguments be always named.



}
\value{

  \code{NULL} (the function is called for its side effect, a plot),
  unless \code{do.plot} and \code{do.lines} are both \code{FALSE}. In
  that case, a list is return:

  \code{list(t, x, axis.pos = pos, axis.labels, timegrid)}

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
t <- as.POSIXct("2012-08-31 08:05:00") - 36000:1
x <- runif(length(t))

par(tck = 0.001, mgp = c(3,1,0.5), bty = "n")
plotTradingHours(x, t,
                 interval = "15 min", labels = "hours",
                 xlab = "time", ylab = "random points",
                 col = "blue")
}
