\name{RiskReturnStatistics}
\alias{drawdown}
\title{
  Risk and Return Statistics for Time Series
}
\description{
  Functions for computing risk/reward statistics for time series (eg,
  price series of mutual funds).

}
\usage{
drawdown(v, relative = TRUE)
}
\arguments{
  \item{v}{
    a price series (returns)
  }
  \item{relative}{
    if \code{TRUE}, maximum drawdown is chosen according to percentage
    losses; else in units of \code{v}
  }
}
\details{
  requires a vector (can be \code{zoo})
}
\value{
  a list
  \item{maximum}{maximum drawdown}
  \item{high}{the max of \code{v}}
  \item{highPosition}{position of \code{high}}
  \item{low}{the min of \code{v}}
  \item{lowPosition}{position of \code{low}}
}
\references{
  GMS
}
\author{
  Enrico Schumann
}

\examples{
v <- cumprod(1+rnorm(20)*0.02)
drawdown(v)
}
