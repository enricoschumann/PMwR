\name{pm}
\alias{pm}
\title{
  Partial Moments
}
\description{
  Compute partial moments.
}
\usage{
  pm(x, xp = 2, threshold = 0, lower = TRUE,
     normalise = FALSE, na.rm = FALSE)
}
\arguments{
  \item{x}{
    a numeric vector or a matrix
  }
  \item{xp}{
    exponent
  }
  \item{threshold}{
    a numeric vector of length one
  }
  \item{lower}{
    logical
  }
  \item{normalise}{
    logical
  }
  \item{na.rm}{
    logical
  }
}
\details{

  For a vector \eqn{x}{\emph{x}} of length \eqn{n}{\emph{n}}, partial
  moments are computed such:

  \deqn{\mathrm{upper\ partial\ moment} = \frac{1}{n} \sum_{x >
      t}\left(x - t \right)^e}{%
    upper partial moment = sum_{x > t}(x - t)^e / n}
  
  \deqn{\mathrm{lower\ partial\ moment} = \frac{1}{n} \sum_{x <
      t}\left(t - x \right)^e}{%
    lower partial moment = sum_{x < t}(t - x)^e / n}

  The \code{threshold} is denoted \eqn{t}{\emph{t}}, the exponent
  \code{xp} is labelled \eqn{e}{\emph{e}}.

  If \code{normalise} is \code{TRUE}, the result is raised to
  \code{1/xp}.  If \code{x} is a matrix, the function will compute the
  partial moments column-wise.
  
}
\value{
  numeric
}
\references{

  Gilli, M., Maringer, D. and Schumann, E. (2011) \emph{Numerical
    Methods and Optimization in Finance}, Chapter 13. Elsevier.
    \url{http://www.elsevierdirect.com/product.jsp?isbn=9780123756626}

}
\author{
  Enrico Schumann
}
\examples{
pm(x <- rnorm(100), 2)
var(x)/2

pm(x, 2, normalise = TRUE)
sqrt(var(x)/2)
}
