\name{PMwR-package}
\alias{PMwR-package}
\alias{PMwR}
\docType{package}
\title{
Functions for the Practical Management of Financial Portfolios
}
\description{
Functions for the practical management of financial portfolios:
computing profit-and-loss, analysing trades, reporting and more.
}
\details{
See the book
}
\author{
Enrico Schumann

Maintainer: Enrico Schumann <es@enricoschumann.net>
}
\references{

Gilli, M., Maringer, D. and Schumann, E. (2011) \emph{Numerical Methods
and Optimization in Finance}. Elsevier.
\url{http://www.elsevierdirect.com/product.jsp?isbn=9780123756626}

Schumann, E. (2013) \emph{Portfolio Management with R}.

} 
\examples{
require("tth")

tex <- c("This is \\\\textbf{bold} and this \\\\textit{italic}.",
         "Points on the unit circle: $x^2 + y^2 = 1$.")
tth(tex)
ttm(tex)
}
\keyword{package}
