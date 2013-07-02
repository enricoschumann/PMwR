\name{journal}
\alias{journal}
\alias{c.journal}
\alias{length.journal}
\alias{print.journal}
\alias{sort.journal}
\alias{subset.journal}
\title{
  Create a Journal
}
\description{
  Create a journal of financial transactions.
}
\usage{
journal(timestamp, amount, price, id, instrument, account, \dots)


\method{c}{journal}(\dots)
\method{length}{journal}(x)
\method{print}{journal}(x, \dots, width = 60L)
\method{sort}{journal}(x, decreasing = FALSE, by = "timestamp", \dots, na.last = TRUE)
\method{subset}{journal}(x, \dots)
\method{as.data.frame}{journal}(x, row.names = NULL, optional = FALSE, \dots)

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
    sort by
}
  \item{na.last}{
    arguments passed to sort
}
}
\details{
  
  The \code{journal} function creates a list of the arguments and
  attaches a class attribute (\sQuote{journal}).  There are few
  methods (\code{\link{print}}, \code{\link{c}}).

  Currently, missing arguments will be coded as \code{\link{NA}} except
  for \code{id} and \code{account} which become \code{NULL}.
  
}
\value{
  
  An object of class \code{journal}, which is simply a list of
  vectors.

}
\references{
  ES - PMwR
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
