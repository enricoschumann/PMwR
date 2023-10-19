\name{rebalance}
\alias{rebalance}
\alias{replace_weight}
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
          current.weights = FALSE,
          target.weights = TRUE)

\method{print}{rebalance}(x, \dots, drop.zero = TRUE)

replace_weight(weights, ..., prefix = TRUE, sep = "::")
}
\arguments{
  \item{current}{%
    the current holdings: a (typically named) vector of position sizes;
    can also be a \code{position}
  }
  \item{target}{%
    the target holdings: a (typically named) vector of weights;
    can also be a \code{position}
  }
  \item{price}{%
    a numeric vector: the current prices; may be named
  }
  \item{notional}{%
    a single number: the value of the portfolio; if missing,
    replaced by \code{sum(current*prices)}
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
    \code{rebalance}: arguments passed to \code{\link{print}};
    \code{replace_weight}: numeric vectors
  }
  \item{drop.zero}{

    logical: should instruments with no difference
    between \code{current} and \code{target} be included?

    Note the different defaults for computing and
    printing.

  }
  \item{current.weights}{
    logical. If \code{TRUE} (the default), the values in \code{current} are
    interpreted as weights. If \code{FALSE}, \code{current} is
    interpreted as a position (i.e. notional/number of contracts).
  }
  \item{target.weights}{
    logical. If \code{TRUE} (the default), the values in \code{target} are
    interpreted as weights. If \code{FALSE}, \code{target} is
    interpreted as a position (i.e. notional/number of contracts).
  }
  \item{weights}{
    a numeric vector with named components
  }
  \item{sep}{
    character
  }
  \item{prefix}{
    logical
  }
}
\details{

  The function computes the necessary trades to move from the
  \code{current} portfolio to a \code{target} portfolio.

  \code{replace_weight} is a helper function to split
  baskets into their components. All arguments passed
  via \code{...} should be named vectors. If names are
  not syntactically valid (see
  \code{\link{make.names}}), quote them. The passed
  vectors themselves should be passed as named
  arguments: see examples.


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
\references{%

  Schumann, E. (2023) \emph{Portfolio Management with R}.
  \url{http://enricoschumann.net/R/packages/PMwR/};
  in particular, see\cr
  \url{http://enricoschumann.net/R/packages/PMwR/manual/PMwR.html#rebalance}

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

## replace_weight: the passed vectors must be named;
##                 'basket_3' is ignored because not
##                 component of weights is named
##                 'basket_3'

replace_weight(c(basket_1 = 0.3,
                 basket_2 = 0.7),
               basket_1 = c(a = 0.1, b = 0.4, c = .5),
               basket_2 = c(x = 0.1, y = 0.4, z = .5),
               basket_3 = c(X = 0.5, Z = 0.5),
               sep = "|")
}
