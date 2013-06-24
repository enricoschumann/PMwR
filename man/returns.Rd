\name{returns}
\alias{returns}
\title{
  Compute Returns.
}
\description{
  Compute returns of price series.
}
\usage{
returns(x, t = NULL, period = "month", complete.first = TRUE,
        pad = NULL,  position)
}
\arguments{
  \item{x}{
    a \code{numeric} vector (possibly with a \code{dim} attribute; ie, a
    matrix)
  }
  \item{t}{
        timestamps. See argument \code{"period"}.
  }
  \item{period}{
       typically character: predefined are "month" and "day". Only
       used if \code{t} is not \code{NULL}.
  }
  \item{complete.first}{
    logical. 
  }
  \item{pad}{
    either \code{NULL} (no padding of initial lost observation) or a value used
    for padding (reasonable values might be \link{NA} or 0)
  }
  \item{position}{
    numeric; the same length/dimension as \code{x}
  }
}
\details{

  Returns


  
}
\value{

  If called as \code{returns(x)} a numeric vector or matrix.

  If called with a time argument, an object of class "preturns" (period
  returns), which is a list of the dates, the returns and the period.
  
}
\author{
  Enrico Schumann
}
\seealso{
  \code{\link{backtest}}
}
\examples{

x <- 101:112
returns(x)
returns(x, pad = NA)

t <- seq(as.Date("2012-01-01"), as.Date("2012-12-31"), by = "1 day")
x <- seq_along(t) + 100
returns(x, t = t, period = "month")
returns(x, t = t, period = "month", complete.first = FALSE)

x <- 101:105
returns(x, position = c(1,1,0,0,1))


## time-weighted returns
price <- c(101,102,103,104)
all.equal(returns(price, position = c(1,1,2,2)),
          returns(price))

pos <- c(1,1,1,2,2,0)
dim(pos) <- c(3,2)
price <- c(100,100,100,100,100,100)
dim(price) <- c(3, 2)
returns(price, position = pos)
all.equal(returns(price, position = pos), returns(price[,1]))

pos1 <- c(1,1,1,2,2,2)
pos2 <- pos1 * 2
dim(pos2) <- dim(pos1) <- c(3,2)
price <- c(101,102,103,103,105,107)
dim(price) <- c(3,2)
all.equal(returns(price, position = pos1),
          returns(rowSums(price * pos1)))
all.equal(returns(price, position = pos1),
          returns(price, position = pos2))


pos <- c(1, 2, 3, 2, 3, 3)
dim(pos) <- c(3, 2)
price <- c(101,102,103,103,105,107)
dim(price) <- c(3,2)
returns(price, position = pos)



}
