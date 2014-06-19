\name{NAVseries}
\alias{NAVseries}
\title{
  Net-asset-value (NAV) series
}
\description{
  Create a net-asset-value (NAV) series.
}
\usage{
NAVseries(NAV, timestamp,
          instrument = NULL, title = NULL, description = NULL)

\method{print}{NAVseries}(x, \dots)
\method{summary}{NAVseries}(object, \dots)
}
\arguments{
  \item{NAV}{
    numeric
}
  \item{timestamp}{
    time stamp (typically \code{\link{Date}} or \code{\link{POSIXct}}
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
  an \code{NAVseries}
}
\item{object}{
  an \code{NAVseries}
}
}
\details{
  Experimental.
}
\value{
A \code{NAVseries}
}
\references{
  ES - PMwR
}
\author{
  Enrico Schumann
}
\seealso{
  \code{\link{journal}}
}
\examples{
## to be added
}
