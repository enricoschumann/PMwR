\name{TextTools}
\alias{TextTools}
\alias{rmspace}
\alias{char2num}
\alias{expstr}
\alias{rmrp}
\title{
  Manipulating Character Vectors
}
\description{  
  Manipulating and (re-)formating character vectors; for example, to transform them into
  numerical vectors.
}
\usage{
rmspace(s, leading = TRUE, trailing = TRUE)
char2num(s, dec = ",", big.mark = ".")
expstr(s, after, width, fill = " ", at)
rmrp(s, pattern, \dots)
}
\arguments{
  \item{s}{a character vector}
  \item{leading}{logical}
  \item{trailing}{logical}
  \item{dec}{
    character: the decimal point; will be replaced with \dQuote{\code{.}} 
  }
  \item{big.mark}{
    character: will be removed, see Details.
  }
  \item{after}{
    character: a pattern, to be passed to \code{\link{regexpr}}
  }
  \item{width}{integer}
  \item{fill}{character}
  \item{at}{integer}
  \item{pattern}{character}
  \item{\dots}{arguments passed to \code{\link{grep}}}
}
\details{

  \code{rmspace} removes leading and trailing space; space is recognised
  through the regular expression \code{\\s}.

  \code{char2num} uses \code{\link{as.numeric}} on a character vector
  after removing/replacing certain characters: all occurences of
  \code{big.mark} are removed, and \code{dec} is changed to a dot.

  \code{expstr} inserts blanks into the elements of a character vector
  such that all elements have the same \code{width} (ie,
  \code{\link{nchar}}).  Note that it will (currently) not contract a
  string, only expand it.

  \code{rmrp} removes a repeated pattern in a character vector (eg,
  repeated blank lines).
  
}
\value{
  a character vector
}
\seealso{
   \code{\link{strwrap}}, \code{\link{format}}
}
\author{
  Enrico Schumann
}
\examples{

s <- c("\t 2 2\n \t", "   ab  ")
rmspace(s)

char2num("12.000,23")
char2num("12,000.23", big.mark = ",")
char2num("12000.23",  big.mark = " ") ## but 'as.numeric' is simpler
char2num("12'000.23", big.mark = "'")
char2num("12 000|23", dec = "|", big.mark = " ")

## expand to width 10, but keep two initial blanks 
s <- c("  A 1", "  B     2")
expstr(s, after = " +[^ ]+ +", width = 10)

## remove repeated blanks from vector
s <- c("* Header", "", "  ","", "** Subheader")
rmrp(s, "^ *$")
}
