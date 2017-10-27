\name{NAVseries}
\alias{NAVseries}
\alias{as.NAVseries}
\alias{plot.NAVseries}
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

\method{summary}{NAVseries}(object, monthly = TRUE, na.rm = FALSE, \dots )

\method{plot}{NAVseries}(x, y, xlab = "", ylab = "", type = "l", \dots )
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
\item{monthly}{
  if \code{TRUE} (default), volatility computations
  are done on monthly returns
}
\item{na.rm}{
  logical
}
\item{y}{
  a second NAVseries to be plotted. Not supported yet.
}
\item{xlab}{
  character
}
\item{ylab}{
  character
}
\item{type}{
  character. See \code{\link{plot}}.
}
}
\details{

  An NAVseries is a numeric vector (the actual series) and additional
  information, attached as attributes: timestamp, instrument, title,
  description. Of these attributes, timestamp is the most useful, as it
  is used for several computations (e.g. when callung
  \code{\link{summary}}) or for plotting.

  The summary method returns a list of the original NAVseries plus
  various statistics, such as return per year and volatility.
    
}
\value{
  an \code{NAVseries}: see Details.

  
}
\references{

  Schumann, E. (2017) \emph{Portfolio Management with
  \R}. \url{http://enricoschumann.net/PMwR/}

} 
\author{
Enrico Schumann <es@enricoschumann.net>
}
\seealso{
  \code{\link{journal}}
}
\examples{
summary(NAVseries(DAX[[1]], as.Date(row.names(DAX)), title = "DAX"))
}
