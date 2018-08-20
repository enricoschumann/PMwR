\name{is_valid_ISIN}
\alias{is_valid_ISIN}
\title{
  Validate International Securities Identification Numbers (ISINs)
}
\description{
  Check whether a given ISIN is valid.
}
\usage{
is_valid_ISIN(isin)
}
\arguments{
  \item{isin}{
    a character vector
  }
}
\details{  

  Checks a character vector of \acronym{ISIN}s. The function returns
  \code{TRUE} if the \acronym{ISIN} is valid; else \code{FALSE}.

  The test procedure in \acronym{ISO} 6166 does not differentiate
  between cases.  Thus, \acronym{ISIN}s are transformed to uppercase
  before being tested.
  
}
\value{
  a logical vector
}
\references{
  \url{http://en.wikipedia.org/wiki/ISO_6166}

  \url{https://www.anna-web.org/standards/isin-iso-6166/}
}
\author{
  Enrico Schumann
}
\examples{
isin <- c("US0378331005", "AU0000XVGZA3",
          "DE000A0C3743", "not_an_isin")
is_valid_ISIN(isin)

is_valid_ISIN(c("US0378331005",
                "us0378331005")) ## case is ignored
}
