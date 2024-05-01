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

  a \code{\link{data.frame}}:
  \item{\code{peak}}{peak before drawdown}
  \item{\code{trough}}{lowest point}
  \item{\code{recover}}{new high or \code{\link{NA}} if the
    drawdown has not been recovered yet}
  \item{\code{max}}{the max drawdown}

}
\seealso{

  The actual computation of the drawdowns is done by function
  \code{\link[NMOF]{drawdown}} in package \pkg{NMOF}.

  Series of uninterrupted up and down movements can be
  computed with \code{\link{streaks}}.

}
\author{
  Enrico Schumann
}
\references{

  Gilli, M., Maringer, D. and Schumann, E. (2019)
  \emph{Numerical Methods and Optimization in Finance}.
  2nd edition. Elsevier. \doi{10.1016/C2017-0-01621-X}

  Schumann, E. (2023) \emph{Portfolio Management with R}.
  \url{https://enricoschumann.net/PMwR/};\cr in particular,\cr
  \url{https://enricoschumann.net/R/packages/PMwR/manual/PMwR.html#drawdowns-streaks}

}
\examples{
x <- c(100, 98)
drawdowns(x)

x <- c(100, 98, 102, 99)
dd <- drawdowns(x)
dd[order(dd$max, decreasing = TRUE), ]
}
