\name{rebalance}
\alias{rebalance}
\alias{print.rebalance}
\title{
  Rebalance portfolio
}
\description{
  Rebalance portfolio  
}
\usage{
rebalance(current, target, prices,
          notional, names = NULL,
          w.current = FALSE, w.target = TRUE,
          do.trunc = TRUE)

\method{print}{rebalance}(x, ... \dots)
}
\arguments{
  \item{current}{
    the current holdings: a vector of position sizes
  }
  \item{target}{
    the target holdings: a vector of weights
  }
  \item{prices}{
    the current prices
  }
  \item{notional}{
    the value of the portfolio; if missing, replaced by \code{sum(current*prices)}
  }
  \item{names}{
    character.
  }
  \item{w.current}{
    are \code{current} positions weights? Default is \code{FALSE}.
  }
  \item{w.target}{
    are \code{target} positions weights? Default is \code{TRUE}.
  }
  \item{do.trunc}{
    truncate computed positions? Default is \code{TRUE}.
  }
  \item{x}{
    an object of class \code{rebalance}.
  }
  \item{\dots}{
    arguments passed to \code{\link{print}}
  }
}
\details{

  Given a desired portfolio and a current portfolio, the function
  computes the necessary trades.
  
}
\value{

  An object of class rebalance, which is a list:
  
  \item{current}{\ldots}
  \item{new}{\ldots}
  \item{names}{\ldots}
  \item{prices}{\ldots}
  \item{notional}{\ldots}

}
\references{
ES
}
\author{
ES
}
\section{Warning }{
  Experimental. The interface may change.
}
\seealso{
  \code{\link{Tradelist}}
}
\examples{
## to be added
}
