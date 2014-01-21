\name{divAdjust}
\alias{divAdjust}
\title{
  Adjust for Dividends
}
\description{
  Adjust a time-series for dividends
}
\usage{
divAdjust(x, t, div, backward = TRUE, additive = FALSE)
}
\arguments{
  \item{x}{
    a numeric vector
}
  \item{t}{
    an integer vector, specifying the positions in \code{x} at which
    dividends were paid (\sQuote{ex-days})
}
  \item{div}{
    a numeric vector, specifying the dividends.  If necessary, recycled
    to the length of \code{t}.
}
  \item{backward}{
    logical
}
  \item{additive}{
    logical
}
}
\details{
  Dividends are reinvested.
}
\value{
  a  numeric vector of length \code{length(x)}
}
%% \references{
%% %% ~put references to the literature/web site here ~
%% }
\author{
  Enrico Schumann
}
%% \note{
%% %%  ~~further notes~~
%% }

%% %% ~Make other sections like Warning with \section{Warning }{....} ~

%% \seealso{
%% %% ~~objects to See Also as \code{\link{help}}, ~~~
%% }
\examples{
x <- c(9.777, 10.04, 9.207, 9.406)
div <- 0.7
t <- 3

divAdjust(x, t, div)
divAdjust(x, t, div, FALSE)
}
