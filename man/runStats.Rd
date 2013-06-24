\name{runStats}
\alias{runStats}
\title{
Running (moving window) statistics
}
\description{
Compute a number of statistics like mean, minimum or maximum of a vector
(typically a time series) on a shifting window. The function emphasises
efficiency; potential problems (like NA values) should be checked before.
}
\usage{
runStats(what, y, N = 5L, pad = NULL, q = NULL, h = NULL)
}
\arguments{
  \item{what}{
a character vector: `mean', `min', `max' (`sum', `abssum' and others
will be added)
}
  \item{y}{
a numeric vector
}
  \item{N}{
an integer
}
  \item{pad}{
replace elements 1 to N-1 in returned vector. Defaults to NULL 
}
  \item{q}{
the quantile (not implemented)
}
  \item{h}{
the order statistic for a quantile (not implemented)
}
}
\details{
  All algorithms are written in \code{C}. There is no error handling,
  and no handling of \code{NA}, \code{Inf} or similar values.
  
  For quantiles either \code{h} or \code{q} must be specified, not both.
}
\value{
  A numeric vector of length \code{length(y)}.  
}
\references{
Gilli, M., Maringer, D. and Schumann, E. (2011) \emph{Numerical Methods and Optimization in Finance}. Elsevier. 
\url{http://www.elsevierdirect.com/product.jsp?isbn=9780123756626}  
}
\author{
Enrico Schumann
}

\seealso{
A number of packages already include such computations. See, for
examples, packages \pkg{caTools} or \pkg{zoo}.
}
\examples{
1+1
}
