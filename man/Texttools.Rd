\name{Texttools}
\alias{Texttools}
\alias{rmspace}
\alias{char2num}
\alias{expstr}
\title{
  Functions to manipulate character vectors
}
\description{
  Manipulating character vectors, typically to transform them into
  numerical vectors.
}
\usage{
rmspace(s)
char2num(s, dec = ",", big.mark = ".")

expstr(s, after, width, fill = " ", at)
}
\arguments{
  \item{s}{a character vector}
  \item{dec}{
    character: the decimal point; will be replaced with \dQuote{\code{.}} 
  }
  \item{big.mark}{
    character: will be removed, see Details.
  }
  \item{after}{
    character: a pattern, to be passed to \code{\link{regepxr}}
  }
  \item{width}{integer}
  \item{fill}{character}
  \item{at}{integer}
}
\details{

  \code{rmspace} removes leading and trailing space; space is recognised
  through the regular expression \code{\\s}.

  \code{char2num} uses \code{\link{as.numeric}} on a character vector
  after removing/replacing certain characters: all occurences of
  \code{big.mark} are removed, and \code{dec} is changed to a dot.

}
\value{
  a character vector
}
\seealso{
   \code{\link{strwrap}}.
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

}
