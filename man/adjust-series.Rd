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
    logical; see Details
  }
  \item{additive}{
    logical; see Details
  }
}
\details{

  The function transforms \eqn{x}{\code{x}} into returns, and
  with those returns specified in \eqn{t}{\code{t}} calculated
  as

  \deqn{\frac{x_t + D_t}{x_{t-1}} - 1\,,}{(x(t) + D(t))/x(t-1) - 1,}

  in which \eqn{x}{x} is the price, \eqn{D}{D} are dividends
  and \eqn{t}{t} is time.  The adjusted \eqn{x}{\code{x}} is
  then reconstructed from those returns.

  When \code{additive} is \code{\link{TRUE}}, dividends are
  simply added back to the series; see Examples.

  With \code{backward} set to \code{\link{TRUE}}, which is the
  default, the final prices in the unadjusted series matches
  the final prices in the adjusted series.

}
\value{
  a numeric vector of length equal to \code{length(x)}
}
\references{

  Schumann, E. (2023) \emph{Portfolio Management with R}.
  \url{https://enricoschumann.net/PMwR/}

  Using \code{div_adjust} for handling generic external cashflows:
  \url{https://enricoschumann.net/R/packages/PMwR/manual/PMwR.html#returns-with-external-cashflows}
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



## 'additive' ** FALSE ** (default setting)
x <- c(100, 95, 100, 95, 100)
div <- 5
t <- c(2, 4)
div_adjust(x, t, div)
## 90.25  90.25  95.00  95.00 100.00
returns(div_adjust(x, t, div))
## 0.00000000 0.05263158 0.00000000 0.05263158
## ==> reflect _actual_ returns 100/95 - 1

## 'additive' ** TRUE **
div_adjust(x, t, div, additive = TRUE)
## 90  90  95  95 100
returns(div_adjust(x, t, div, additive = TRUE))
## 0.00000000 0.05555556 0.00000000 0.05263158
## ==> reflect return 95/90 - 1
}
