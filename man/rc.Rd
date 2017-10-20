\name{rc}
\alias{rc}
\title{
  Return Contribution
}
\description{
  Return contribution of portfolio segments.
}
\usage{
rc(R, weights, timestamp, segments = NULL)
}
\arguments{
  \item{R}{
    returns: a numeric matrix
}
  \item{weights}{

    the segment weights: a numeric matrix. \code{weights[i,j]} must
    correspond to \code{R[i,j]}

}
\item{timestamp}{
  character or numeric
}
\item{segments}{
  
  character. If missing, column names of \code{R} or of \code{weights}
  are used (if they are not \code{NULL}).
  
}
}
\details{

  The function computes segment contribution,
  potentially over time. Returns and weights must be
  arranged in matrices, with rows corresponding to time
  periods and columns to portfolio segments

  Weights can be missing, in which case \code{R} is
  assumed to already comprise segment returns.
  
}
\value{
  A list of two components
 \item{period_contributions}{a data.frame}
 \item{total_contributions}{a numeric vector}
}
\references{

  Feibel, Bruce (2003), \emph{Investment Performance Measurement}, Wiley.

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
}
