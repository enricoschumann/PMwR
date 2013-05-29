\name{Mailtools}
\alias{saveAttachment}
\title{
  Tools for Email Handling
}
\description{
  Tools for Email Handling
}
\usage{
saveAttachment(pattern, fname, writeIfExists = TRUE, adjustfn = TRUE)
}

\arguments{
  \item{pattern}{
    the attachment to find. Can be a regular expression.
}
  \item{fname}{
the mbox file
}
  \item{writeIfExists}{
    logical
}
  \item{adjustfn}{
    default is \code{TRUE}: adjust filenames on Windows? 
}

}
\details{
  
The function is currently only tested with mbox files (which is the
format that Thunderbird uses, or the format of \code{nnfolders} in
Gnus. The function will load the whole mbox file into memory, which may
not work very well if the file is large.

}
\value{
  
  The function is called for its side effect. It returns a character
  vector of matched attachment names.

}
\references{
  http://www.gnu.org/software/emacs/manual/html_node/gnus/Mail-Folders.html
}
\author{
  Enrico Schumann
}
\examples{
## to be added
}
