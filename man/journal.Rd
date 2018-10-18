\name{journal}
\alias{journal}
\alias{aggregate.journal}
\alias{all.equal.journal}
\alias{as.journal}
\alias{c.journal}
\alias{head.journal}
\alias{is.journal}
\alias{journal.default}
\alias{length.journal}
\alias{print.journal}
\alias{sort.journal}
\alias{split.journal}
\alias{subset.journal}
\alias{summary.journal}
\alias{tail.journal}
\alias{[.journal}
\alias{[<-.journal}
\alias{as.data.frame.journal}
\title{
  Journal
}
\description{
  Create and manipulate a journal of financial transactions.
}
\usage{
journal(amount, \dots)

as.journal(x, \dots)

is.journal(x)

\method{journal}{default}(amount, price, timestamp, instrument,
        id = NULL, account = NULL, \dots)

\method{c}{journal}(\dots, recursive = FALSE)

\method{length}{journal}(x)

\method{aggregate}{journal}(x, by, FUN, \dots)

\method{print}{journal}(x, \dots,
      width = getOption("width"), max.print = getOption("max.print"),
      exclude = NULL, include.only = NULL)

\method{sort}{journal}(x, decreasing = FALSE, by = "timestamp", \dots, na.last = TRUE)

\method{subset}{journal}(x, \dots)

\method{[}{journal}(x, i, match.against = NULL,
                    ignore.case = TRUE, \dots, reverse = FALSE)

\method{[}{journal}(x, i, match.against = NULL,
                    ignore.case = TRUE, \dots, reverse = FALSE) <- value

\method{as.data.frame}{journal}(x, row.names = NULL, optional = FALSE, \dots)

\method{head}{journal}(x, n = 6L, \dots, by = TRUE)

\method{tail}{journal}(x, n = 6L, \dots, by = TRUE)

}
\arguments{
  \item{timestamp}{
    An atomic vector of mode numeric or character. Timestamps should
    typically be sortable.
  }
  \item{amount}{
    numeric
  }
  \item{price}{
    numeric
  }
  \item{instrument}{
    character or numeric (though typically character)
  }
  \item{id}{
    An atomic vector. Default is \code{NULL}.
  }
  \item{account}{
    An atomic vector. Default is \code{NULL}. 
  }
  \item{\dots}{    
    For \code{journal}: further arguments, which must all be named.

    For \code{subset}: an expression that evaluates to a logical vector. The
    expression may use all fields of the passed journal; see Examples.

    For \code{`[`}: arguments other than \code{ignore.case} to be passed
    to \code{\link{grep}}.

    For \code{sort}: arguments passed to \code{\link{sort}}.
  }
  \item{x}{    

    a \code{journal} or an object to be coerced to class \code{journal} (for
    \code{as.journal}) or to be checked if it inherits from journal (for
    \code{is.journal})

  }
  \item{width}{
    integer. See \code{\link{options}}.
  }
  \item{decreasing}{
    passed to \code{\link{sort}}
  }
  \item{by}{
    \code{sort}: sort by field. \code{head}/\code{tail}: by instrument.
  }
  \item{ignore.case}{
    logical
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
    character: print only those fields. (Not supported yet.) 
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
    integer, logical or character.  The latter is interpreted as a
  regexp (see \code{\link{grep}})
  }
  \item{n}{
    integer.
  }
  \item{match.against}{
    character vector of field names. Default is \code{NULL}, which
    means to match against all character fields.
  }
  \item{reverse}{
    logical. If \code{TRUE}, select journal entries that do not match
    regular expression.
  }
  \item{FUN}{
    either a function that takes as input a journal and evaluates to a
    journal, or a list of named functions
  }
  \item{value}{
    a replacement value
  }
}
\details{
  
  The \code{journal} function creates a list of the
  arguments and attaches a class attribute
  (\sQuote{journal}). It is a generic function; the
  default method creates a journal from atomic
  vectors. The \code{btest} method extracts the journal
  from the results of a backtest; see
  \code{\link{btest}}.

  \code{as.journal} coerces an object to a journal;
  mostly used for creating a journal from a
  \code{\link{data.frame}}.

  \code{journal} methods are available for several
  generic functions, for instance:

  \describe{

    \item{\code{all.equal}}{compare contents of two
    journals}

    \item{\code{aggregate}}{Splits a journal according
    to \code{by}, applies a function to every
    sub-journal and recombines the results into a
    journal.}

    \item{\code{as.data.frame}}{coerces journal to
    \code{\link{data.frame}}}
  
    \item{\code{c}}{Combine several journals into one.
      Note that the first argument to \code{c.journal}
      must inherit from \code{journal}, or else the
      method dispatch will fail.  For empty journals,
      use \code{journal()} (not \code{NULL}).}

    \item{\code{length}}{number of transactions in a
    journal; it uses the length of \code{amount}.}

    \item{\code{split}}{Splits a journal according to
    \code{f}, yielding a list of journals.  Often used
    interactively to have information per sub-journal
    printed. }

    \item{\code{subset}}{evaluates an expression in an
    environment that can access all fields of the
    journal.  The function is meant for interactive
    analysis; care is needed when it is used within
    other functions: see Examples and the Manual.}

    \item{\code{summary}}{provides summary statistics,
    such as number of trades and average buy/sell
    prices}

    \item{\code{toOrg}}{converts a journal to an Org
  table; package \pkg{orgutils} must be available}

  }  
  
  For journals that have a length, missing arguments will be coded as
  \code{\link{NA}} except for \code{id} and \code{account}, which become
  \code{\link{NULL}}. In zero-length (i.e. \sQuote{empty}) journals, all
  fields have length 0. A zero-length journal is created, of instance,
  by saying \code{journal()} or when an zero-row \code{data.frame} is
  passed to \code{as.journal}.
  
}
\value{
  
  An object of class \code{journal}, which is a list of atomic
  vectors.

}
\references{

Schumann, E. (2018) \emph{Portfolio Management
  with R}. \url{http://enricoschumann.net/R/packages/PMwR/}

} 
\author{
Enrico Schumann <es@enricoschumann.net>
}
\seealso{
  \code{\link{position}}, \code{\link{pl}}
}
\examples{
j <- journal(timestamp = 1:3,
             amount = c(1,2,3),
             price = 101:103,
             instrument = c("Stock A", "Stock A", "Stock B"))

## *** subset *** in functions
##   this should work as expected ...
t0 <- 2.5                         
subset(j, timestamp > t0)

##   ... but here?!
tradesAfterT <- function(j, t0)   
    subset(j, timestamp > t0)        
tradesAfterT(j, 0)               

##   if really required
tradesAfterT <- function(j, t0) {
    e <- substitute(timestamp > t0, list(t0 = t0))
    do.call(subset, list(j, e))
}
tradesAfterT(j, 0)

##   ... or much simpler
tradesAfterT <- function(j, t0)
    j[j$timestamp > t0]
tradesAfterT(j, 0)

## *** aggregate ***
##   several buys and sells on two days
##   aim: find average buy/sell price per day
j <- journal(timestamp = structure(c(15950, 15951, 15950, 15951, 15950,
                                     15950, 15951, 15951, 15951, 15951),
                                   class = "Date"),
             amount = c(-3, -4, -3, -1, 3, -2, 1, 3, 5, 3),
             price = c(104, 102, 102, 110, 106, 104, 104, 106, 108, 107),
             instrument = c("B", "B", "A", "A", "B", "B", "A", "B", "A", "A"))

by <- list(j$instrument, sign(j$amount), as.Date(j$timestamp))
fun <- function(x) {
    journal(timestamp = as.Date(x$timestamp[1]),
            amount = sum(x$amount),
            price = sum(x$amount*x$price)/sum(x$amount),
            instrument = x$instrument[1L])
}
aggregate(j, by = by, FUN = fun)

}
