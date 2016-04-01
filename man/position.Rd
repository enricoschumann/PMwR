\name{position}
\alias{position}
\alias{position.default}
\alias{print.position}
\title{
  Aggregate Transactions to Position
}
\description{
  Use information on trades to compute a position at a specific point
  in time.
}
\usage{
position(amount, \dots)

\method{position}{default}(amount, timestamp, instrument, when, drop.zero = FALSE, \dots)

\method{print}{position}(x, \dots, sep = NA)

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
  \item{x}{    
    An object of type position.
  }
  \item{\dots}{
    arguments passed to \code{\link{print}}
  }
  \item{sep}{    
    A regular expression. Split instruments accordingly. \strong{Not
    implemented yet.}
  }
}
\details{

  Only \code{instrument} can be specified, so to compute positions on an
  \code{account} basis, paste \code{account} to \code{instrument}, or
  split the journal.

  The function checks if \code{timestamp} is sorted (see
  \code{\link{is.unsorted}}) and sorts by timestamp, if required.
  
}
\value{
  An object of class \code{position}, which  is a numeric matrix with
  \code{instrument} and \code{timestamp} attributes. Note that \code{position}
  will never drop the result's \code{dim} attribute; it will always be a matrix.
  
}
\references{
  Enrico Schumann -- Portfolio Management with R
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
