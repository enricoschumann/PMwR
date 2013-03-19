\name{position}
\alias{position}
\alias{position.default}
\alias{position.Tradelist}
\title{
  Aggregate Trade Data into a Position
}
\description{
  Use information on trades  to compute a position at a specific point
  in time.
}
\usage{
position(notional, \dots)

\method{position}{Tradelist}(tlist, when, from, to, \dots)

\method{position}{default}(notional,tradetimes, when, from, to, \dots)
}
\arguments{
  \item{when}{
    a timestamp or a vector of timestamps
  }
  \item{tlist}{
    a \code{\link{Tradelist}}
  }
  \item{\dots}{
  }
  \item{notional}{
    numeric
  }
  \item{tradetimes}{
    times (sortable)
  }
  \item{from}{
    timestamp
  }
  \item{to}{
    timestamp
  }
}
\details{
to be added
}
\value{
  a numeric vector or matrix
}
\references{
  ES -- PMwR
}
\author{
  Enrico Schumann
}
\seealso{
  \code{\link{Tradelist}}
}
\examples{
t <- 1:5
n <- c(1,1,-1,3,-4)
position(notional=n, tradetimes=t, when = 4.9)
}
