\name{position}
\alias{position}
\title{
  Aggregate Transactions to Position
}
\description{
  Use information on trades to compute a position at a specific point
  in time.
}
\usage{
position(amount, timestamp, instrument, when, drop.zero = FALSE)
}
\arguments{
  \item{when}{
    a timestamp or a vector of timestamps
  }
  \item{amount}{
    numeric or an object of class \code{\link{journal}}
  }
  \item{timestamp}{
    timestamps (must be sortable)
  }
  \item{instrument}{
    character: symbols to identify different instruments
  }
  \item{drop.zero}{
    logical: drop instruments that have a zero position
  }  
  \item{\dots}{
  }
}
\details{

  Only \code{instrument} can be specified, so to compute positions on an
  \code{account} basis, paste \code{account} to \code{instrument}.

  The function checks if \code{timestamp} is sorted (see
  \code{\link{is.unsorted}}) and sorts by timestamp, if required.
  
}
\value{
  An object of class \code{position}.
}
\references{
  ES -- PMwR
}
\author{
  Enrico Schumann
}
\seealso{
  \code{\link{journal}}
}
\examples{
position(amount = c(1, 1, -1, 3, -4), timestamp = 1:5, when = 4.9)

## using a journal
J <- journal(timestamp = 1:5, amount = c(1, 1, -1, 3, -4))
position(J, when = 4.9)
}
