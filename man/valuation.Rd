\name{valuation}
\alias{valuation}
\alias{valuation.journal}
\alias{valuation.position}
\title{
  Valuation
}
\description{
  Valuation of financial objects: map an object into a quantity that is
  measured in some unit.
}
\usage{
valuation(x, ...)

\method{valuation}{journal}(x,  multiplier = 1, cashflow = function(x) x$amount * 
    x$price, flip.sign = TRUE, \dots)

\method{valuation}{position}(x, multiplier = 1, price.table, do.sum, unit, \dots)

}
\arguments{
  \item{x}{
    an object
}
  \item{multiplier}{
    a numeric vector, typically with named elements
}
  \item{cashflow}{
    either a numeric vector or a function that takes on argument (a
    journal) and transforms it into a numeric vector
}
  \item{flip.sign}{
    logical.  If \code{TRUE} (the default), a positive amount gets
    mapped into a negative cashflow.
}
  \item{price.table}{
    numeric
  }
  \item{do.sum}{
    logical: sum over positions
  }
  \item{unit}{
    not implemented
  }
  \item{\dots}{
    other arguments passed to methods
}
}
\details{
  \emph{valuation} is a generic function.  Its semantics suggest that an
  object (eg, a financial instrument or a position) is mapped into a
  concrete quantity (such as an amount of some currency).
}
\value{
  depends on the object
}
%% \references{
%%   %% ~put references to the literature/web site here ~
%% }
\author{
  Enrico Schumann
}
\note{
  \bold{Very experimental.}
}
%% \seealso{
%% %% ~~objects to See Also as \code{\link{help}}, ~~~
%% }
\examples{
## ... will be provided when the function has become stable
}
