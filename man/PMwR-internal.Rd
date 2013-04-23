\name{PMwR-internal}
\alias{PMwR-internal}
\alias{makeHHMMSS}
\alias{wait}
\alias{first}
\alias{last}
\alias{rebalance}
\title{Internal (non-exported) functions in the PMwR package}
\description{

  Several internal functions, not exported from the \pkg{PMwR} name
  space.
  
}
\usage{
makeHHMMSS(x, label = "time specification (HHMMSS)")
wait(x)
first(x, by, index = FALSE)
last(x, by, index = FALSE)
rebalance(current, target, prices, notional,
          w.target = TRUE, names = NULL)
}
\details{

  \code{makeHHMMSS} transforms the character \code{x} into a
  character vector that resembles a format \code{HHMMSS}. 

  If the argument to \code{wait} is a single number, the function will
  call \code{\link{Sys.sleep}} with this number. If \code{x} is a
  vector, a uniform random variate between \code{min(x)} and
  \code{max(x)} is passed to \code{\link{Sys.sleep}}.
  
}
\author{
  Enrico Schumann
}
\examples{
dates <- seq(as.Date("2012-01-01"), as.Date("2013-02-05"), by = "1 day")
PMwR:::last(dates,  by = format(dates, "\%Y\%m"))
PMwR:::first(dates, by = format(dates, "\%Y\%m"))

s <- c("DTE","SAP","SDF","MEO","MRKK","FRE","HEN3" ,"BEI"  ,"LIN"  ,"FME")
current <- c(500  , 0   ,90   ,0    ,20    ,34   ,16     ,49     ,0      ,0)
target  <- c(0.15,0.0,0.1,0,0.15,0.15,0.15,0.15,0,0.15)
prices <- c(8.36,63.44 ,35.9325 ,21.2325 ,113.85 ,95.21 ,65.965 ,69.48 ,132.175 ,51.55)
PMwR:::rebalance(current, target, prices, notional = 50000, names= s)

}
\keyword{internal}
