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

  \acronym{ISO} 6166 is silent when it comes to upper/lower case of
  letters, but the test procedure does not differentiate between cases.
  Thus, \acronym{ISIN}s are transformed to uppercase before being
  tested.
  
}
\value{
  a logical vector
}
\references{
  \url{http://en.wikipedia.org/wiki/ISO_6166}
  \url{http://www.anna-web.com/index.php/home/isinsaiso6166}
}
\author{
  Enrico Schumann
}
\examples{
isin <- c("US0378331005", "AU0000XVGZA3", "DE000A0C3743", "ss")
isValidISIN(isin)
isValidISIN("US0378331005")
}
