\name{pl}
\alias{pl}
\title{
  Profit and Loss
}
\description{
  Profit and (or) loss of financial transactions. 
}
\usage{
pl(amount, price, instrument = NULL, timestamp = NULL,
   along.timestamp = FALSE, do.sort = FALSE,
   initial.cash = 0, initial.position = NULL,
   initial.price, current.price, multiplier,
   tol = 1e-10, do.warn = TRUE)
}
\arguments{
  \item{amount}{
    notionals
  }
  \item{price}{
    a vector of prices
  }
  \item{instrument}{
    a character vector
  }
  \item{timestamp}{
    a vector. 
  }
  \item{along.timestamp}{
    a logical
  }
  \item{do.sort}{
    sort all by time
  }
  \item{initial.cash}{
    a numeric vector of length one
  }
  \item{initial.position}{
    a \code{\link{position}}
  }
  \item{current.price}{price.from}
  \item{initial.price}{price.to}
  \item{multiplier}{not supported}
  \item{tol}{numeric}
  \item{do.warn}{logical}
}
\details{

  Computes profit and/or loss.
  
}
\value{

  \code{pl}: an object of class \code{pl}, which is a list of five
  elements (and the class attribute).

}
\author{
  Enrico Schumann
}
\seealso{
  \code{\link{btest}},   \code{\link{returns}}
}
\examples{
J <- journal(timestamp = c(1, 2, 3),
                amount = c(1, 1, -2),
                price  = c(100,102, 101))
pl(J)
pl(c(1, 1, -2), c(100,102, 101)) ## without a 'journal'


J <- journal(timestamp  = c(1,2,3,1,2,3),
             amount     = c(1, 1, -2, 1,1,-2),
             price      = c(100, 102, 101, 100, 102, 105),
             instrument = c(rep("A", 3), rep("B", 3)))
pl(J)}
