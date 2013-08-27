\name{journal}
\alias{journal}
\alias{c.journal}
\alias{length.journal}
\alias{print.journal}
\alias{sort.journal}
\alias{subset.journal}
\alias{aggregate.journal}
\alias{split.journal}
\alias{head.journal}
\alias{tail.journal}
\alias{[.journal}
\alias{as.data.frame.journal}
\title{
  Journal
}
\description{
  Create a journal of financial transactions.
}
\usage{
journal(timestamp, amount, price, instrument,
        id = NULL, account = NULL, \dots)

\method{c}{journal}(\dots, recursive = FALSE)

\method{length}{journal}(x)

\method{print}{journal}(x, \dots, width = 60L, max.print = 100,
                       exclude = NULL, include.only = NULL)

\method{sort}{journal}(x, decreasing = FALSE, by = "timestamp", \dots, na.last = TRUE)

\method{subset}{journal}(x, \dots)

\method{[}{journal}(x, i, match.against = NULL)

\method{as.data.frame}{journal}(x, row.names = NULL, optional = FALSE, \dots)

\method{head}{journal}(x, n = 6L, \dots, by = TRUE)

\method{tail}{journal}(x, n = 6L, \dots, by = TRUE)

}
\arguments{
  \item{timestamp}{
    time
  }
  \item{amount}{
    volume per trade
  }
  \item{price}{
    trade price
  }
  \item{id}{
    trade id
  }
  \item{instrument}{
    instrument
  }
  \item{account}{
    account 
  }
  \item{\dots}{
    
    For \code{journal}: further arguments, which must all be named. For
    \code{subset}: an expression that evaluates to a logical vector; it
    can use all fields of the passed journal; see Examples.
    
  }
  \item{x}{    
    a \code{journal} object    
  }
  \item{width}{
    integer
  }
  \item{decreasing}{
    arguments passed to sort
  }
  \item{by}{
    \code{sort}: sort by field. \code{head}/\code{tail}: by instrument.
  }
  \item{na.last}{
    arguments passed to sort
  }
  \item{max.print}{
    maximum number of transactions to print
  }
  \item{exclude}{
    character: fields that should not be printed
  }
  \item{include.only}{
    character: print only those fields
  }
  \item{row.names}{
    see \code{\link{as.data.frame}}
  }
  \item{optional}{
    see \code{\link{as.data.frame}}    
  }
  \item{recursive}{
    ignored (see \code{\link{c}})
  }
  \item{i}{
    integer, logical or character.  The latter is interpreted as a regexp.
  }
  \item{n}{
    integer.
  }
  \item{match.against}{
    character vector
  }
}
\details{
  
  The \code{journal} function creates a list of the arguments and
  attaches a class attribute (\sQuote{journal}).  There are several
  methods:

  \describe{
    \item{\code{aggregate}}{Splits a journal according to by, applies a
    function to every sub-journal and recombines the results into a
    journal.}
  
    \item{\code{split}}{Splits a journal according to \code{f}, yielding
    a list of journals.  (Often used interactively to have information
    per sub-journal \code{print}ed }

    \item{\code{c}}{Combine several journals into one.  Note that the
      first argument to \code{c.journal} must be a journal, or else method
      dispatch will fail.  For empty journals, use \code{journal()} (not
      \code{NULL}).}
    
    \item{\code{length}}{number of transactions in a journal; it uses
    the length of \code{amount}.}
  }  

  
  Missing arguments will be coded as \code{\link{NA}} except for
  \code{id} and \code{account} which become \code{\link{NULL}}.
  
}
\value{
  
  An object of class \code{journal}, which is simply a list of atomic
  vectors.

}
\references{
  Enrico Schumann -- Portfolio Management with R
}
\author{
  Enrico Schumann
}
\seealso{
  \code{\link{pl}}
}
\examples{

t1 <- journal(timestamp = "2013-01-14 12:00:00",
                amount = c(9000, 60000, 6500, 9000, 4500, 5500, 9000),
                price = c(61.23, 9.04, 82.52, 60.76, 130.5622, 101.50, 61.16),
                instrument = c("bei", "dte", "fre", "hen3",  "lin",  "mrk", "sap"),
                account = "P1")

t2 <- journal(timestamp = "2013-02-05 12:00:00",
                amount = c(7000, 50000, 3500, 5500, 7500, 4500, 7000),
                price = c(64.8606, 8.7785, 133.0005, 91.6590, 65.5398, 102.0192,60.4702),
                instrument = c("bei", "dte", "lin", "fre",  "hen3",  "mrk", "sap"),
                account = "P2")

c(t1, t2)

subset(t1, amount > 10000)

}
