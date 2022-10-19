\name{is_valid_ISIN}
\alias{is_valid_ISIN}
\alias{is_valid_SEDOL}
\title{
  Validate Security Identification Numbers
}
\description{
  Check whether a given ISIN or SEDOL is valid.
}
\usage{
is_valid_ISIN(isin)
is_valid_SEDOL(SEDOL, NA.FALSE = FALSE)
}
\arguments{
  \item{isin}{
    a character vector
  }
  \item{SEDOL}{
    a character vector
  }
  \item{NA.FALSE}{
    logical
  }
}
\details{

  Checks a character vector of \acronym{ISIN}s and \acronym{SEDOL}s. The
  function returns \code{TRUE} if the \acronym{ISIN} is valid; else
  \code{FALSE}.

  International Securities Identification Numbers (ISINs): The test
  procedure in \acronym{ISO} 6166 does not differentiate between cases.
  Thus, \acronym{ISIN}s are transformed to uppercase before being
  tested.

}
\value{

  A named logical vector. For \code{is_valid_SEDOL}, a character vector
  is attached as an attribute \code{note}.

}
\references{
  \url{https://en.wikipedia.org/wiki/ISO_6166}

  \url{https://en.wikipedia.org/wiki/SEDOL}

  \url{https://anna-web.org/identifiers/}
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


SEDOL <- c("0263494", "B1F3M59", "0263491", "A", NA)
is_valid_SEDOL(SEDOL)
## 0263494 B1F3M59 0263491       A    <NA>
##    TRUE    TRUE   FALSE   FALSE      NA

is_valid_SEDOL(SEDOL, NA.FALSE = TRUE)
## 0263494 B1F3M59 0263491       A    <NA>
##    TRUE    TRUE   FALSE   FALSE   FALSE

}
