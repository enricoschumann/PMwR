\name{rebalance}
\alias{rebalance}
\alias{print.rebalance}
\title{
  Rebalance Portfolio
}
\description{
  Compute the differences between two portfolios.  
}
\usage{
rebalance(current, target, price,
          notional = NULL, multiplier = 1,
          truncate = TRUE, match.names = TRUE,
          fraction = 1, drop.zero = FALSE,
          target.weights = TRUE)

\method{print}{rebalance}(x, \dots, drop.zero = TRUE)
}
\arguments{
  \item{current}{
    the current holdings: a (typically named) vector of position sizes;
    can also be a \code{position}
  }
  \item{target}{
    the target holdings: a (typically named) vector of weights;
    can also be a \code{position}
  }
  \item{price}{
    the current prices
  }
  \item{notional}{    
    the value of the portfolio; if missing, replaced by
    \code{sum(current*prices)}
  }
  \item{multiplier}{
    numeric vector, possibly named
  }
  \item{truncate}{
    truncate computed positions? Default is \code{TRUE}.
  }
  \item{match.names}{
    logical
  }
  \item{fraction}{
    numeric
  }
  \item{x}{
    an object of class \code{rebalance}.
  }
  \item{\dots}{
    arguments passed to \code{\link{print}}
  }
  \item{drop.zero}{
    logical; note the different defaults for computing and printing
  }
  \item{target.weights}{
    logical. If \code{TRUE} (the default), the values in target are
    interpreted as weights. If \code{FALSE}, \code{target} is
    interpreted as a position (i.e. notional/number  of contracts).
  }
}
\details{

  The function computes the necessary trades to move from the
  \code{current} portfolio to a \code{target} portfolio.
  
}
\value{

  An object of class \code{rebalance}, which is a
  \code{data.frame}:
  
  \item{instrument}{character, or \code{NA} when
                    \code{match.names} is \code{FALSE}}
  \item{price}{prices}
  \item{current}{current portfolio}
  \item{target}{new portfolio}
  \item{difference}{the difference between current and target}

  Attached to the data.frame are several attributes:
  
  \item{notional}{notional}
  \item{match.names}{logical}
  \item{multiplier}{multipliers}
  
}
\references{

  Schumann, E. (2016) \emph{Portfolio Management with
    R}. \url{http://enricoschumann.net/R/packages/PMwR/}

}
\author{
  Enrico Schumann
}
\seealso{
  \code{\link{journal}}
}
\examples{
r <- rebalance(current = c(a = 100, b = 20),
               target  = c(a = 0.2, c = 0.3),
               price   = c(a = 1, b = 2, c = 3))
as.journal(r)
}
