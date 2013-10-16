\name{toText}
\alias{toText}
\alias{toText.default}
\title{
  Convert Objects to Plain Text
}
\description{
  Converts an object into (plain) text.
}
\usage{
toText(x, \dots)

\method{toText}{default}(x, \dots)
}
\arguments{
  \item{x}{
    an object
}
  \item{\dots}{
     arguments passed to methods
}
}
\details{
  
   A generic function.  Method are expected to coerce a given object to
  lines of text that can be used, for instance, for reports.  There is
  no restriction on encoding, so plain text does not necessarily mean
  \acronym{ascii}.  But current methods do not map into
  markup-representations.

}
\value{
  a character vector (lines of text)
}
\references{
  Enrico Schumann -- Portfolio Management with R
}
\author{
  Enrico Schumann
}
\seealso{
  \code{\link{toLatex}}, \code{\link{toHTML}}
}
\examples{
toText(c("a", "b", "c"))
}
