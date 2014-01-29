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
rebalance(current, target, price,
          notional = NULL, 
          truncate = TRUE,
          match.names = TRUE,
          check.match = TRUE)

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
    the value of the portfolio; if missing, replaced by \code{sum(current*prices)}
  }
  \item{truncate}{
    truncate computed positions? Default is \code{TRUE}.
  }
  \item{match.names}{
    logical
  }
  \item{check.match}{
    logical
  }
  \item{x}{
    an object of class \code{rebalance}.
  }
  \item{\dots}{
    arguments passed to \code{\link{print}}
  }
  \item{drop.zero}{
    logical
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
%% \references{
%% Enrico Schumann - Portfoliomanagement with R
%% }
\author{
  Enrico Schumann
}
\section{Warning }{
  Very experimental. The interface may change.
}
%% \seealso{
%%   \code{\link{journal}}
%% }
\examples{
## to be added
}
