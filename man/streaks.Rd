\name{streaks}
\alias{streaks}
\alias{streaks.default}
\alias{streaks.zoo}
\alias{streaks.NAVseries}
\title{
  Up and Down Streaks
}
\description{
  Compute up and down streaks for time-series.
}
\usage{
streaks(x, ...)

\method{streaks}{default}(x, up =  0.2, down = -0.2,
                initial.state = NA, \dots)
\method{streaks}{zoo}(x, up =  0.2, down = -0.2,
                initial.state = NA, \dots)
\method{streaks}{NAVseries}(x, up =  0.2, down = -0.2,
                initial.state = NA, \dots)

}
\arguments{
  \item{x}{
    a price series
}
  \item{initial.state}{
    \code{\link{NA}}, \code{"up"} or \code{"down"}
}
  \item{up}{
    a number, such as 0.1 (i.e. 10\%)
}
  \item{down}{
    a negative number, such as -0.1 (i.e. -10\%)
}
  \item{\dots}{
    other arguments passed to methods
}
}
\details{

  \code{streaks} is a generic function. It computes
  series of uninterrupted up and down movements in a
  price series. Uninterrupted is meant in the sense
  that no countermovement of \code{down} (\code{up})
  percent or more occurs in up (down) movements.
  
  There are methods for numeric vectors, and
  \code{\link{NAVseries}} and \code{zoo} objects.

  
}
\value{
  A \code{\link{data.frame}}:
 \item{start}{beginning of streak}
 \item{end}{end of streak}
 \item{state}{\code{up}, \code{down} or \code{\link{NA}}.}
}
%% \references{
%% %% ~put references to the literature/web site here ~
%% }
\author{
  Enrico Schumann
}
\seealso{
  \code{\link{drawdowns}}
}
\examples{
\dontrun{
library("PMwR")
library("zoo")
dax <- zoo(DAX[[1]], as.Date(row.names(DAX)))
streaks(dax)
}
}
