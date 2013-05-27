\name{position}
\alias{position}
\title{
  Aggregate Trade Data into a Position
}
\description{
  Use information on trades to compute a position at a specific point
  in time.
}
\usage{
position(amount,timestamp, instrument, when, from, to, \dots)
}
\arguments{
  \item{when}{
    a timestamp or a vector of timestamps
  }
  \item{\dots}{
  }
  \item{amount}{
    numeric
  }
  \item{timestamp}{
    times (sortable)
  }
  \item{instrument}{
    character. Symbols to identify different instruments
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
  \code{\link{Journal}}
}
\examples{
position(amount = c(1,1,-1,3,-4),
         timestamp = 1:5, when = 4.9)

## using a Journal
lst <- Journal(timestamp = 1:5,
               amount = c(1,1,-1,3,-4))
position(lst, when = 4.9)
}
