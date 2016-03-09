\name{pl}
\alias{pl}
\alias{.pl}
\alias{pl.default}
\alias{pl.journal}
\alias{print.pl}
\alias{as.data.frame.pl}
\title{
  Profit and Loss
}
\description{
  Compute profit and (or) loss of financial transactions. 
}
\usage{
pl(amount, \dots)

\method{pl}{default}(amount, price, timestamp = NULL,
                     instrument = NULL, multiplier = 1,
                     along.timestamp = FALSE, approx = FALSE,
                     initial.position = NULL, initial.price = NULL,
                     eval.price = NULL, tol = 1e-10, \dots)

\method{pl}{journal}(amount, multiplier = 1,
                     along.timestamp = FALSE, approx = FALSE,
                     initial.position = NULL, initial.price = NULL,
                     eval.price = NULL, tol = 1e-10, \dots)

.pl(amount, price, tol = 1e-10, do.warn = TRUE)

\method{print}{pl}(x, \dots, use.crayon = NULL)

\method{as.data.frame}{pl}(x, \dots)
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
  %% \item{do.sort}{
  %%   sort all by time
  %% }
  \item{initial.position}{
    a \code{\link{position}}. \bold{Not yet supported}.
  }
  \item{initial.price}{
    prices to evaluate initial position. \bold{Not yet supported}
  }
  \item{eval.price}{

    numeric vector. With several instruments, the prices should be
    named, e.g. \code{c(stock1 = 100, stock2 = 101)}.

  }
  \item{multiplier}{not supported yet}
  \item{approx}{logical}
  \item{tol}{numeric: threshold to consider a position zero.}
  \item{do.warn}{logical}
  \item{x}{a pl object to be printed or to be coerced to a data.frame}
  \item{\dots}{further argument}
  \item{use.crayon}{logical}
}
\details{

  Computes profit and/or loss.

  To use package crayon -- which is only sensible in interactive use --,
   either explicitly set it to \code{TRUE} or set an option
   \code{PMwR.use.crayon} to \code{TRUE}.

  
}
\value{

  For \code{pl}, an object of class \code{pl}, which is a list of lists:
  one list for each instrument. Each such list contains numeric vectors:
  \code{pl}, \code{realised}, \code{unrealised}, \code{buy},
  \code{sell}, \code{volume}.

}
\author{
  Enrico Schumann
}
\seealso{
  \code{\link{btest}},   \code{\link{returns}}
}
\examples{
J <- journal(timestamp = c(  1,   2,   3),
             amount    = c(  1,   1,  -2),
             price     = c(100, 102, 101))
pl(J)
pl(c(1, 1, -2), c(100,102, 101))  ## without a 'journal'


J <- journal(timestamp  = c(  1,   2,   3,   1,   2,   3),
             amount     = c(  1,   1,  -2,   1,   1,  -2),
             price      = c(100, 102, 101, 100, 102, 105),
             instrument = c(rep("Bond A", 3), rep("Bond B", 3)))
pl(J)
as.data.frame(pl(J))



## Example for 'eval.price'
instrument  <- c(rep("Bond A", 2), rep("Bond B", 2))
amount <- c(1, -2, 2, -1)
price <- c(100, 101, 100, 105)

## ... no p/l + warning because positions not closed:
pl(amount, price, instrument = instrument) 

## ... but with eval.price specified, p/l is computed:
pl(amount, price, instrument = instrument, ## p/l + no warning
   eval.price = c("Bond A" = 103, "Bond B" = 100)) 

### ... and is, except for volume, the same as here:
instrument  <- c(rep("Bond A", 3), rep("Bond B", 3))
amount <- c(1, -2, 1, 2, -1, -1)
price <- c(100, 101, 103, 100, 105, 100)
pl(amount, price, instrument = instrument) ## no p/l + warning


## use package 'crayon'
\dontrun{
options(PMwR.use.crayon = FALSE)
pl(amount = c(1, -1), price = c(1, 2))
options(PMwR.use.crayon = TRUE)
pl(amount = c(1, -1), price = c(1, 2))
}

}
