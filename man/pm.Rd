\name{pm}
\name{alias}
\title{
  Partial Moments
}
\description{
  Compute partial moments
}
\usage{
  pm(x, xp = 2, threshold = 0, lower = TRUE,
     keep.sign = FALSE, normalise = FALSE)
}
\arguments{
  \item{x}{
    a numeric vector
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
  \item{keep.sign}{
    logical
  }
  \item{normalise}{
    logical
  }

}
\details{
  requires a vector
}
\value{

  numeric
}
\references{

  Gilli, M., Maringer, D. and Schumann, E. (2011) \emph{Numerical
    Methods and Optimization in Finance}. Elsevier.
    \url{http://www.elsevierdirect.com/product.jsp?isbn=9780123756626}

}
\author{
  Enrico Schumann
}
\examples{
pm(x <-rnorm(100), 2)
var(x)/2

pm(x, 2, normalise = TRUE)
sqrt(var(x)/2)
}
