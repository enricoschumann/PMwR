\name{drawdown}
\alias{drawdown}
\title{
  Drawdown
}
\description{
  Compute the drawdown of a time series.
}
\usage{
  drawdown(v, relative = TRUE, summary = TRUE)
}
\arguments{
  \item{v}{
    a price series (a numeric vector)
  }
  \item{relative}{
    if \code{TRUE}, maximum drawdown is chosen according to percentage
    losses; else in units of \code{v}
  }
  \item{summary}{
    if \code{TRUE}, provide maximum drawdown and time when it occured;
    else return drawdown vector
  }
}
\details{

  The drawdown at position \emph{t} of a time series \emph{v} is the
  difference between the highest peak that was reached before \emph{t}
  and the current value.  If the current value represents a new high,
  the drawdown is zero.


}
\value{

  If \code{summary} is \code{FALSE}, a vector of the same length as
  \code{v}.  If \code{summary} is \code{TRUE}, a list

  \item{maximum}{maximum drawdown}
  \item{high}{the max of \code{v}}
  \item{high.position}{position of \code{high}}
  \item{low}{the min of \code{v}}
  \item{low.position}{position of \code{low}}
}
\references{

  Gilli, M., Maringer, D. and Schumann, E. (2011) \emph{Numerical
    Methods and Optimization in Finance}, Chapter 13. Elsevier.
    \url{http://www.elsevierdirect.com/product.jsp?isbn=9780123756626}

}
\author{
  Enrico Schumann
}
\examples{
v <- cumprod(1 + rnorm(20) * 0.02)
drawdown(v)
}
