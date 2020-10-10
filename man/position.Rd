\name{position}
\alias{position}
\alias{position.default}
\alias{position.journal}
\alias{print.position}
\alias{as.matrix.position}
\title{
  Aggregate Transactions to Positions
}
\description{

  Use information on single trades to compute a
  position at a specific point in time.

}
\usage{
position(amount, \dots)

\method{position}{default}(amount, timestamp, instrument, when,
                 drop.zero = FALSE, account = NULL,
                 use.names = NULL, \dots)

\method{position}{journal}(amount, when, drop.zero = FALSE,
                           use.account = FALSE, \dots)

\method{print}{position}(x, \dots, sep = ":")

}
\arguments{
  \item{when}{
    a timestamp or a vector of timestamps;
    alternatively, several keywords are supported. See Details.
  }
  \item{amount}{
    numeric or an object of class \code{\link{journal}}
  }
  \item{timestamp}{
    numeric or character: timestamps, must be sortable
  }
  \item{instrument}{
    character: symbols to identify different instruments
  }
  \item{account}{
    character: description of account
  }
  \item{use.account}{
    logical
  }
  \item{use.names}{
    logical
  }
  \item{drop.zero}{

    If logical, drop instruments that have a zero
    position; default is \code{FALSE}. If numeric, it
    is used as a tolerance; e.g., a value of
    \code{1-e12} will drop any position whose absolute
    amount is smaller than \code{1-e12}.

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

  \code{position} is a generic function; most useful is
  the method for \code{\link{journal}}s.
  
  The function checks if \code{timestamp} is sorted (see
  \code{\link{is.unsorted}}) and sorts the journal by \code{timestamp},
  if required. If there are (some) \code{NA} values in \code{timestamp},
  but \code{timestamp} is sorted otherwise, the function will proceed
  (with a warning, though).

  The argument \code{when} can also be specified as one of several
  keywords: \code{last} (or \code{newest} or \code{latest}) provides the
  position at the latest timestamp; \code{first} (or \code{oldest})
  provides the position at the earliest timestamp; \code{all} provides
  the positions at all timestamps in the journal. \code{endofday},
  \code{endofmonth} and \code{endofyear}
  provide positions at the end of all calendar days, months and years within the
  timestamp range of the journal. The latter keywords can only work if
  \code{timestamp} can be coerced to \code{\link{Date}}.
    
}
\value{

  An object of class \code{position}, which is a numeric matrix with
  \code{instrument} and \code{timestamp} attributes. Note that
  \code{position} will never drop the result's \code{dim} attribute: it
  will always be a matrix of size \code{length(when)} times
  \code{length(unique(instrument))}, which may not be obvious from the
  printed output.

  To extract the numeric position matrix, say \code{as.matrix(p)}.
  
  
  
}
\references{

  Schumann, E. (2018) \emph{Portfolio Management
    with \R}. \url{http://enricoschumann.net/R/packages/PMwR/}
  
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
