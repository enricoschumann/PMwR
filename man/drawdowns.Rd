\name{drawdowns}
\alias{drawdowns}
\alias{drawdowns.zoo}
\alias{drawdowns.default}
\title{
  Compute Drawdowns
}
\description{
  Compute drawdown statistics.
}
\usage{
drawdowns(x, \dots)
\method{drawdowns}{default}(x, \dots)
\method{drawdowns}{zoo}(x, \dots)
}
\arguments{
  \item{x}{
    a \code{numeric} vector  of prices
  }
  \item{\dots}{
    additional arguments, to be passed to methods
  }
}
\details{

  \code{drawdowns} is a generic function. It computes drawdown
  statistics: maximum; and time of peak, trough and recovery.

}
\value{

  a \code{\link{data.frame}}
  
}
\author{
  Enrico Schumann
}
\seealso{

  the actual computation of the drawdowns is done by function
  \code{\link[NMOF]{drawdown}} in package \pkg{NMOF}

}
\examples{

x <- c(100, 98)
drawdowns(x)

x <- c(100, 98, 102, 99)
dd <- drawdowns(x)
dd[order(dd$max, decreasing = TRUE), ]
}
