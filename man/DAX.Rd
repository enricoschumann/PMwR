\name{DAX}
\alias{DAX}
\docType{data}
\title{
  Deutscher Aktienindex (DAX)
}
\description{
  Historical Prices of the DAX.
}
\usage{data("DAX")}
\format{
  A data frame with 505 observations on the following variable:
  \describe{
    \item{\code{DAX}}{a numeric vector}
  }
}
\details{

  The \acronym{DAX} (\emph{Deutscher Aktienindex}) is a
  stock-price index of the largest companies listed in
  Germany.  The dataset comprises the close prices of
  the index for the years 2014 and 2015; dates are
  provided as rownames.

}
%% \source{
%% %%  ~~ reference to a publication or URL from which the data were obtained ~~
%% }
%% \references{
%% %%  ~~ possibly secondary sources and usages ~~
%% }
\examples{
str(DAX)
summary(DAX)
}
\keyword{datasets}
