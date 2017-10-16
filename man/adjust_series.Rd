\name{AdjustSeries}
\alias{div_adjust}
\alias{split_adjust}
\title{
  Adjust Time Series for Dividends and Splits
}
\description{
  Adjust a time series for dividends and splits.
}
\usage{
div_adjust(x, t, div, backward = TRUE, additive = FALSE)
split_adjust(x, t, ratio, backward = TRUE)
}
\arguments{
  \item{x}{
    a numeric vector
}
  \item{t}{
    an integer vector, specifying the positions in \code{x} at which
    dividends were paid (\sQuote{ex-days}) or at which a split occurs
}
  \item{div}{
    a numeric vector, specifying the dividends.  If necessary, recycled
    to the length of \code{t}.
}
\item{ratio}{
  
    a numeric vector, specifying the split ratios.  A 2-for-1 stock
    split, say, has a ratio of 2.  (This is \sQuote{American Style}; in
    some countries, for instance Germany, a 2-for-1 stock split would be
    called a 1-for-1 split: you keep your shares and receive one new
    share per share that you own.)
    
}
\item{backward}{
  logical
}
\item{additive}{
  logical
}
}
\details{

  With \code{backward} set to \code{TRUE}, which is the default, the
  final prices in the unadjusted series matches the final prices in the
  adjusted series.

}
\value{
  a numeric vector of length \code{length(x)}
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
%% \seealso{
%% %% ~~objects to See Also as \code{\link{help}}, ~~~
%% }
\examples{
x <- c(9.777, 10.04, 9.207, 9.406)
div <- 0.7
t <- 3

div_adjust(x, t, div)
div_adjust(x, t, div, FALSE)
}
