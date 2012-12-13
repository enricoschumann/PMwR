\name{Mailtools}
\alias{saveAttachment}
\title{
Functions to handle emails
}
\description{
Functions that help with emails
}
\usage{
saveAttachment(pattern, fname, writeIfExists = TRUE)
}

\arguments{
  \item{pattern}{
    the attachment to find. Can be a reg exp.
}
  \item{fname}{
the mbox file
}
  \item{writeIfExists}{
    logical
}
}
\details{
The function will load the whole mbox file into memory.
}
\value{
Called for its side effect.
}
\references{
  ES -- PM with R
}
\author{
  Enrico Schumann
}
\examples{
1+1
}
