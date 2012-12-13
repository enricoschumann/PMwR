\name{isValidISIN}
\alias{isValidISIN}
\title{
  Validate International Securities Identification Numbers (ISINs)
}
\description{
  Check whether a given ISIN is valid.
}
\usage{
isValidISIN(isin)
}
\arguments{
  \item{isin}{
    a character vector
  }
}
\details{  
  Checks a character vector of \acronym{ISIN}s. The function returns
  \code{TRUE} if the \acronym{ISIN} is valid; else \code{FALSE}.
}
\value{
  a logical vector
}
\references{
  \url{http://www.anna-web.com/index.php/home/isinsaiso6166}
}
\author{
  Enrico Schumann
}
\examples{
isin <- c("US0378331005", "AU0000XVGZA3", "DE000A0C3743", "")
isValidISIN(isin)
}
