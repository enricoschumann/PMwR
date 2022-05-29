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

\method{streaks}{default}(x, up =  0.2, down = -up,
        initial.state = NA, y = NULL, relative = TRUE, \dots)
\method{streaks}{zoo}(x, up =  0.2, down = -up,
        initial.state = NA, y = NULL, relative = TRUE, \dots)
\method{streaks}{NAVseries}(x, up =  0.2, down = -up,
        initial.state = NA, bm = NULL, relative = TRUE, \dots)

}
\arguments{
  \item{x}{%
    a price series
  }
  \item{initial.state}{%
    \code{\link{NA}}, \code{"up"} or \code{"down"}
  }
  \item{up}{%
    a number, such as 0.1 (i.e. 10\%)
  }
  \item{down}{%
    a negative number, such as -0.1 (i.e. -10\%)
  }
  \item{y}{%
    another price series
  }
  \item{bm}{%

    another price series. Mapped to \sQuote{\code{y}}
    in the default method.

  }
  \item{relative}{%
    logical
  }
  \item{\dots}{%
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
 \item{state}{\code{up}, \code{down} or \code{\link{NA}}}
 \item{return,change}{the return over the streak. If \code{y}
   was specified, geometric excess return is
   computed (see Examples). If \code{relative} is \code{FALSE},
   the column is named \code{change}.}
}
%% \references{
%% %% ~put references to the literature/web site here ~
%% }
\author{
  Enrico Schumann <es@enricoschumann.net>
}
\seealso{
  \code{\link{drawdowns}}
}
\examples{
streaks(DAX[[1]], t = as.Date(row.names(DAX)))

## results <- streaks(x = <...>, y = <...>)
##
## ===> *arithmetic* excess returns
##       x[results$end]/x[results$start] -
##       y[results$end]/y[results$start]
## ===> *geometric* excess returns
##       x[results$end]/x[results$start] /
##      (y[results$end]/y[results$start]) - 1
}
