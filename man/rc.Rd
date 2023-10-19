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
  \item{R}{returns: a numeric matrix}

  \item{weights}{%
    the segment weights: a numeric matrix.
    \code{weights[i, j]} must correspond to \code{R[i, j]}
  }

  \item{timestamp}{character or numeric}

  \item{segments}{%
    character. If missing, column names of \code{R} or of
    \code{weights} are used (if they are not \code{NULL}).
  }

  \item{method}{a string}

  \item{linking.method}{\code{NULL} or a string}

  \item{allocation.minus.bm}{logical}

  \item{tol}{%
    numeric: weights whose absolute value is below
    \code{tol} are considered zero and not used for
    computations. Ignored if \code{\link{NA}}.
  }

  If portfolio returns are to be compared against
  benchmark returns, benchmark returns/weights must be
  supplied:

  \item{R.bm}{%
    returns: a numeric matrix
  }

  \item{weights.bm}{%
    the segment weights: a numeric matrix.
    \code{weights[i, j]} must correspond to \code{R[i, j]}
  }

}
\details{

  The function computes segment contribution, potentially
  over time. Returns and weights must be arranged in
  matrices, with rows corresponding to time periods and
  columns to portfolio segments.  If \code{weights} and
  \code{R} are not matrices (i.e. are atomic vectors), then
  they are interpreted as cross-sectional weights/returns
  for a single period.

  Weights can be missing, in which case \code{R} is
  assumed to already comprise segment returns.
}
\value{
  A list of two components
 \item{period_contributions}{a data.frame}
 \item{total_contributions}{a numeric vector}
}
\references{

  Jon A. Christopherson and David R. Cari\enc{ñ}{n}o
  and Wayne E. Ferson (2009),
  \emph{Portfolio Performance Measurement and Benchmarking},
  McGraw-Hill.

  Feibel, Bruce (2003),
  \emph{Investment Performance Measurement},
  Wiley.

  \url{http://enricoschumann.net/R/packages/PMwR/manual/PMwR.html#return-contribution}
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
