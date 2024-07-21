\encoding{UTF-8}
\name{rc}
\alias{rc}
\title{
  Return Contribution
}
\description{
  Return contribution of portfolio segments.
}
\usage{
rc(R, weights, timestamp, segments = NULL,
   R.bm = NULL, weights.bm = NULL,
   method = "contribution",
   linking.method = NULL,
   allocation.minus.bm = TRUE,
   tol = sqrt(.Machine$double.eps))
}
\arguments{
  \item{R}{%
    returns: a numeric matrix
  }
  \item{weights}{%
    the segment weights: a numeric matrix.
    \code{weights[i, j]} must correspond to \code{R[i, j]}
  }
  \item{timestamp}{%
    character or numeric
  }
  \item{segments}{%
    character. If missing, column names of \code{R} or of
    \code{weights} are used (if they are not \code{NULL}).
  }

  \item{method}{a string; default is \code{contribution}}

  \item{linking.method}{\code{NULL} or a string. Currently
    supported are \code{0-cumulative, 1-cumulative, 0.5-cumulative}
    (\code{geometric{0,1,0.5}}) and \code{logarithmic}. See Examples.}

  \item{allocation.minus.bm}{logical}

  \item{tol}{%
    numeric: weights whose absolute value is below
    \code{tol} are considered zero and not used for
    computations. Ignored if \code{\link{NA}}.
  }

  If portfolio returns are to be compared against benchmark
  returns, benchmark returns and weights must be supplied:

  \item{R.bm}{%
    benchmark returns: a numeric matrix
  }
  \item{weights.bm}{%
    the benchmark weights of segments: a numeric matrix.
    \code{weights.bm[i, j]} must correspond to \code{R.bm[i, j]}
  }
}
\details{%

  The function computes segment contribution, potentially
  over time.  Returns and weights must be arranged in
  matrices, with rows corresponding to time periods and
  columns to portfolio segments.  If \code{weights} and
  \code{R} are atomic vectors, then they are interpreted as
  cross-sectional weights/returns for a single period,
  i.e. they are handled like row vectors.

  Weights can be missing, in which case \code{R} is assumed
  to already comprise segment returns.

  Note that the segment contributions need not come from
  asset classes; the computation works for any additive
  single-period decomposition of portfolio returns.

}
\value{%

  A list of two components:
  \item{period_contributions}{a data.frame of single-period
   contributions, sorted in time}
 \item{total_contributions}{a numeric vector}

}
\references{

  David R. Cari\enc{ñ}{n}o (1999).
  Combining Attribution Effects Over Time.
  Journal of Performance Measurement.
  \strong{3} (4), 5--14.

  Jon A. Christopherson and David R. Cari\enc{ñ}{n}o and
  Wayne E. Ferson (2009),
  \emph{Portfolio Performance Measurement and Benchmarking},
  McGraw-Hill.

  Feibel, Bruce (2003), \emph{Investment Performance Measurement},
  Wiley.

  Erik Valtonen (2002).
  Incremental Attribution with and without Notional Portfolios.
  Journal of Performance Measurement.
  \strong{7} (1), 68--83.


  \url{https://enricoschumann.net/R/packages/PMwR/manual/PMwR.html#return-contribution}
}
\author{
  Enrico Schumann
}
\seealso{
  \code{\link{returns}}
}
\examples{
weights <- rbind(c( 0.25, 0.75),
                 c( 0.40, 0.60),
                 c( 0.25, 0.75))

R <- rbind(c( 1  ,    0),
           c( 2.5, -1.0),
           c(-2  ,  0.5))/100

rc(R, weights, segment = c("equities", "bonds"))



## EXAMPLE of Christopherson et al., ch 19
weights <- cbind(stocks = c(0.5,  0.55),
                 bonds  = c(0.5, 0.45))
##      stocks bonds
## [1,]   0.50  0.50
## [2,]   0.55  0.45

R <- cbind(stocks = c(.4, 0.1),
           bonds  = c(.1, 0.2))
##      stocks bonds
## [1,]    0.4   0.1
## [2,]    0.1   0.2

## ==> contributions grow at portfolio rate-of-return
rc(R, weights, linking.method =  "geometric1")

## ==> contributions are made on top of current portfolio-value
rc(R, weights, linking.method =  "geometric0")

## ==> mixture
rc(R, weights, linking.method =  "geometric0.5")



## EXAMPLE from
## https://quant.stackexchange.com/questions/36520/
##         how-to-calculate-the-annual-contribution-of-a-fund-to-a-portfolio-of-funds/
##         36530#36530
## (unbreak the URL)

weights <- rbind(c( 0.5, 0.5),
                 c( 0.5, 0.5))

R <- rbind(c( 10,   0),
           c( 0 ,  10))/100


rc(R, weights, segment = c("F1", "F2"), timestamp = 1:2,
   linking.method = "geometric1")
## ==> F1 contributed first, and so gets a higher total
##     contribution

rc(R, weights, segment = c("F1", "F2"), timestamp = 1:2,
   linking.method = "geometric0")
## ==> F2 contributed later, and so gets a higher total
##     contribution because it started off a higher base
##     value




## contribution for btest:
##   run a portfolio 10\% equities, 90\% bonds
P <- as.matrix(merge(DAX, REXP, by = "row.names")[, -1])
(bt <- btest(prices = list(P),
            signal = function() c(0.1, 0.9),
            convert.weights = TRUE,
            initial.cash = 100))

W <- bt$position*P/bt$wealth
rc(returns(P)*W[-nrow(W), ])$total_contributions
}
