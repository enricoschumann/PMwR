\name{Tradelist}
\alias{Tradelist}
\title{
  Create a Trade List
}
\description{
  Create a Tradelist.
}
\usage{
Tradelist(datetime, notional, price, id, instrument, account)
}
\arguments{
  \item{datetime}{
    time
}
  \item{notional}{
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
}
\details{
  Creates a list of the arguments and attaches a class attribute.  
}
\value{
  A \code{Tradelist}.
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

## example Tradelist
x1 <- Tradelist(datetime = "20130114120000",
               notional = c(9000, 60000, 6500, 9000, 4500, 5500, 9000),
               price = c(61.23, 9.04, 82.52, 60.76, 130.5622, 101.50, 61.16),
               instrument = c("bei", "dte", "fre", "hen3",  "lin",  "mrk", "sap"),
               account = "P1")

x2 <- Tradelist(datetime = "20130205120000",
                notional = c(7000, 50000, 3500, 5500, 7500, 4500, 7000),
                price = c(64.8606, 8.7785, 133.0005, 91.6590, 65.5398, 102.0192,60.4702),
                instrument = c("bei", "dte", "lin", "fre",  "hen3",  "mrk", "sap"),
                account = "P2")

x3 <- Tradelist(datetime = "20130215120000",
                notional = c(22000, -8000, 16000),
                price = c(24.0063, 131.3805, 33.5474),
                instrument = c("meo", "lin", "sdf"),
                account = "P3")

}
