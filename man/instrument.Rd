\name{instrument}
\alias{instrument}
\alias{instrument<-}
\title{
  Retrieve or Change Instrument
}
\description{
  Generic function for retrieving and changing instrument information.
}
\usage{
instrument(x, \dots)

instrument(x, \dots) <- value
}
\arguments{
  \item{x}{
    an object
  }
  \item{\dots}{
    arguments passed to methods
    
  }
  \item{value}{
    an object
  }    
}
\details{

  Generic function: extract or, if meaningful, replace
  instrument information
  
}
\value{

  when used to extract instrument, a character vector
  
}
\references{

  Schumann, E. (2017) \emph{Portfolio Management with
    R}. \url{http://enricoschumann.net/R/packages/PMwR/manual/PMwR.html}

}
\author{
  Enrico Schumann
}
\seealso{
  \code{\link{position}}
}
\examples{
jnl <- journal(instrument = "A",
               amount = 100,
               price = 1)
instrument(jnl)

}
