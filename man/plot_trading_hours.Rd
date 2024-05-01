\name{plot_trading_hours}
\alias{plotTradingHours}
\alias{plot_trading_hours}
\title{
  Plot Time Series During Trading Hours
}
\description{
  Plot a time series after removing weekends and specific times of the day.
}
\usage{
plot_trading_hours(x, t = NULL, interval = "5 min",
                   labels = "hours", label.format = NULL,
                   exclude.weekends = TRUE, holidays = NULL,
                   fromHHMMSS = "000000", toHHMMSS = "240000",
                   do.plot.axis = TRUE,
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
    \code{plot_trading_hours}: can be \dQuote{hour}, \dQuote{day},
    \dQuote{dayhour} or \dQuote{month}.
  }
  \item{label.format}{
    See \code{\link{strftime}}.
  }
  \item{exclude.weekends}{
    logical: default is \code{TRUE}
  }
  \item{holidays}{
    a vector of class \code{\link{Date}} or a character
    vector in a format that is understood by \code{\link{as.Date}}.
  }
  \item{fromHHMMSS}{
    a character vector of length one in format \dQuote{HHMMSS}
  }
  \item{toHHMMSS}{
    a character vector of length one in format \dQuote{HHMMSS}
  }
  \item{do.plot.axis}{
    logical. Should \code{axis(1)} be plotted? Default is \code{TRUE}.
  }
  \item{\dots}{
    parameters passed to \code{\link{plot}}
    (and typically \code{\link{par}})
  }
  \item{from}{
    POSIXct: start plot at (if not specified,
    plot starts at first data point)
  }
  \item{to}{
    POSIXct: end plot at (if not specified,
    plot ends at last data point)
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

  Plot a timeseries during specific times of day.

}
\value{

  A list (invisibly if \code{do.plot} is \code{TRUE}):

  \code{list(t, x, axis.pos = pos, axis.labels, timegrid)}

 \item{t}{positions}
 \item{x}{values}
 \item{axis.pos}{positions of x-tickmarks}
 \item{axis.labels}{labels at x-ticks}
 \item{timegrid}{a POSIXct vector}
 \item{map}{a function. See the manual (a link is under References).}


}
\references{

  B.D. Ripley and K. Hornik. \emph{Date-Time Classes}. R-News,
  \strong{1}(2):8--12, 2001.

  E. Schumann (2023) \emph{Portfolio Management with R}.
  \url{https://enricoschumann.net/PMwR/}; in particular, see\cr
  \url{https://enricoschumann.net/R/packages/PMwR/manual/PMwR.html#plot-trading-hours}

}
\author{
Enrico Schumann <es@enricoschumann.net>
}
\seealso{
  \code{\link{DateTimeClasses}}
}
\examples{
t <- as.POSIXct("2012-08-31 08:00:00") + 0:32400
x <- runif(length(t))

par(tck = 0.001, mgp = c(3,1,0.5), bty = "n")
p <- plot_trading_hours(x, t,
                        interval = "5 min", labels = "hours",
                        xlab = "time", ylab = "random points",
                        col = "blue")


## with ?lines
t <- as.POSIXct("2012-08-31 10:00:00") + 0:9000
x <- seq(0, 1, length.out = 9001)
lines(p$map(t)$t, x[p$map(t)$ix], pch = 19)

}
\keyword{chron}
\keyword{hplot}
\keyword{ts}
