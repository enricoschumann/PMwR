\name{Adjust-Series}
\alias{div_adjust}
\alias{split_adjust}
\title{
  Adjust Time Series for Dividends and Splits
}
\description{
  Adjust a time series for dividends and splits.
}
\usage{
div_adjust(x, t, div, backward = TRUE, additive = FALSE)

split_adjust(x, t, ratio, backward = TRUE)
}
\arguments{
  \item{x}{
    a numeric vector: the series to be adjusted
  }
  \item{t}{

    An integer vector, specifying the positions in
    \code{x} at which dividends were paid
    (\sQuote{ex-days}) or at which a split occurred.
    Timestamps may be duplicated, e.g. several payments
    may occur on a single timestamp.

  }
  \item{div}{

    A numeric vector, specifying the dividends (or
    payments, cashflows).  If necessary, recycled to
    the length of \code{t}.

  }
  \item{ratio}{

    a numeric vector, specifying the split ratios. The ratio must be
    \sQuote{American Style}: a 2-for-1 stock split, for example,
    corresponds to a ratio of 2. (In other countries, for instance
    Germany, a 2-for-1 stock split would be called a 1-for-1 split: you
    keep your shares and receive one new share per share that you own.)

  }
  \item{backward}{
    logical
  }
  \item{additive}{
    logical
  }
}
\details{

  With \code{backward} set to \code{TRUE}, which is the
  default, the final prices in the unadjusted series
  matches the final prices in the adjusted series.

}
\value{
  a numeric vector of length equal to \code{length(x)}
}
\references{

  Schumann, E. (2021) \emph{Portfolio Management with R}.
  \url{http://enricoschumann.net/PMwR/}

  Using \code{div_adjust} for handling generic external cashflows:
  \url{http://enricoschumann.net/R/packages/PMwR/manual/PMwR.html#returns-with-external-cashflows}
}
\author{
  Enrico Schumann
}
\examples{
x <- c(9.777, 10.04, 9.207, 9.406)
div <- 0.7
t <- 3

div_adjust(x, t, div)
div_adjust(x, t, div, FALSE)

## assume there were three splits: adjust shares outstanding
shares <- c(100, 100, 200, 200, 1000, 1500)
t <- c(3, 5, 6)
ratio <- c(2, 5, 1.5)
### => invert ratio
split_adjust(shares, t, 1/ratio)
## [1] 1500 1500 1500 1500 1500 1500

split_adjust(shares, t, 1/ratio, backward = FALSE)
## [1] 100 100 100 100 100 100
}
