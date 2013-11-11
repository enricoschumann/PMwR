\name{value}
\alias{value}
\alias{value.journal}
\alias{value.position}
\title{
  Valuation
}
\description{
  Valuation of financial objects: map an object into a quantity that is
  measured in some unit.
}
\usage{
value(x, ...)

\method{value}{journal}(x,  multiplier = 1, cashflow = function(x) x$amount * 
    x$price, flip.sign = TRUE, \dots)

\method{value}{position}(x, multiplier = 1, price.table, do.sum, unit, \dots)

}
\arguments{
  \item{x}{
    an object
}
  \item{\dots}{
    other arguments passed to methods
}
}
\details{
  \emph{value} is a generic function.  Its semantics suggest that an
  object (eg, a financial instrument or a position) is mapped into a
  concrete cquantity (such as an amount of some currency).
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
  Very experimental.
}
%% \seealso{
%% %% ~~objects to See Also as \code{\link{help}}, ~~~
%% }
\examples{
## see methods
}
