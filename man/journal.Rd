\name{Journal}
\alias{Journal}
\alias{print.Journal}
\alias{c.Journal}
\alias{length.Journal}
\title{
  Create a Trade List
}
\description{
  Create a Journal.
}
\usage{
Journal(timestamp, amount, price, id, instrument, account, \dots)

\method{print}{Journal}(x, \dots)
\method{length}{Journal}(x)
\method{c}{Journal}(\dots)
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
    instrument id
}
  \item{account}{
     account id
}
  \item{dots}{
    further arguments; they must all be named
}
  \item{x}{
    a \code{Journal} object
}
}
\details{
  
  The \code{Journal} function creates a list of the arguments and
  attaches a class attribute (\sQuote{Journal}).  There are few
  methods (\code{\link{print}}, \code{\link{c}}).

}
\value{
  
  An object of class \code{Journal}, which is simply a list of
  vectors.

}
\references{
  ES
}
\author{
  Enrico Schumann
}
\seealso{
  \code{\link{PL}}
}
\examples{

t1 <- Journal(timestamp = "2013-01-14 12:00:00",
                amount = c(9000, 60000, 6500, 9000, 4500, 5500, 9000),
                price = c(61.23, 9.04, 82.52, 60.76, 130.5622, 101.50, 61.16),
                instrument = c("bei", "dte", "fre", "hen3",  "lin",  "mrk", "sap"),
                account = "P1")

t2 <- Journal(timestamp = "2013-02-05 12:00:00",
                amount = c(7000, 50000, 3500, 5500, 7500, 4500, 7000),
                price = c(64.8606, 8.7785, 133.0005, 91.6590, 65.5398, 102.0192,60.4702),
                instrument = c("bei", "dte", "lin", "fre",  "hen3",  "mrk", "sap"),
                account = "P2")

c(t1, t2)


}
