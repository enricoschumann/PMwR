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
  series of uninterrupted up and down movements
  (\sQuote{streaks}) in a price series. Uninterrupted
  is meant in the sense that no countermovement of
  \code{down} (\code{up}) percent or more occurs in up
  (down) movements.

  There are methods for numeric vectors, and
  \code{\link{NAVseries}} and \code{zoo} objects.

  The turning points (extreme points) are computed with
  the benefit of hindsight: the starting point (the
  low) of an up streak can only be determined once the
  streak is triggered, i.e. the up streak has already
  run its minimum amount. Vice versa for down streaks.

  When \sQuote{\code{up}} and \sQuote{\code{down}} are
  not equal, results may be inconsistent: in the
  current implementation, \code{streaks} alternates
  between up and down streaks. Suppose \code{up} is
  large compared with \code{down}, i.e. it takes long
  to trigger up streaks, but they are easily
  broken. Down streaks, on the other hand, are quickly
  triggered but rarely broken. Now suppose that a down
  streak is broken by an up streak: it may then well be
  that the up streak would never have been counted as
  such, because it was actually broken itself by
  another down streak. The implementation for differing
  values of \sQuote{\code{up}} and \sQuote{\code{down}}
  may change in the future.

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
