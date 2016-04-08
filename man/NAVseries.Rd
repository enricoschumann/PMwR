\name{NAVseries}
\alias{NAVseries}
\alias{as.NAVseries}
\alias{print.NAVseries}
\alias{summary.NAVseries}
\title{
  Net-asset-value (NAV) series
}
\description{
  Create a net-asset-value (NAV) series.
}
\usage{
NAVseries(NAV, timestamp,
          instrument = NULL, title = NULL, description = NULL)

as.NAVseries(x, ...)

\method{print}{NAVseries}(x, \dots)
\method{summary}{NAVseries}(object, \dots)
}
\arguments{
  \item{NAV}{
    numeric
}
  \item{timestamp}{
    time stamp (typically \code{\link{Date}} or \code{\link{POSIXct}})
}
  \item{instrument}{
    character
}
  \item{title}{
    character
}
  \item{description}{
    character
}
\item{x}{
  an \code{NAVseries} or an object to be coerced to NAVseries
}
\item{object}{
  an \code{NAVseries}
}
\item{\dots}{
  further arguments
}
}
\details{
  Experimental.
}
\value{
A \code{NAVseries}
}
\references{
Schumann, E. (2016) \emph{Portfolio Management
  with R}. \url{http://enricoschumann.net/R/packages/PMwR/manual/PMwR.html}

} 
\author{
Enrico Schumann <es@enricoschumann.net>
}
\seealso{
  \code{\link{journal}}
}
\examples{

\dontrun{
require("tseries")
dax <- get.hist.quote("^GDAXI", quote = "Close",
                      start = as.Date("1999-1-1"))
summary(as.NAVseries(dax))
}
}
